{-|
Module      : Database.RedisGraph
Description : Interact with RedisGraph database
Copyright   : (c) bruno cuconato, 2021
License     : BSD-3
Maintainer  : bcclaro+haskell@gmail.com
Stability   : experimental

Helper functions to work with the RedisGraph graph database. Relies heavily on
the @hedis@ library (you must know how to use @hedis@ to use @hedisgraph@).
-}

{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Database.RedisGraph
  ( Node(..), Path(..), Record, Relationship(..), QueryResult(..), Value(..)
  , query, queryP
  , Redis.runRedis)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Database.Redis (Redis, Reply(..))
import qualified Database.Redis as Redis

type Label = Integer
type Property = Integer

data Node
  = Node { _id :: !Integer
         , labels :: [Label]
         , props :: [(Property, Value)]
         }
  deriving stock (Eq, Show)

data Relationship
  = Relationship { _id :: !Integer
                 , label :: !Label
                 , src :: !Integer
                 , dst :: !Integer
                 , props :: [(Property, Value)]
                 }
  deriving stock (Eq, Show)

data Path
  = Path { nodes :: [Node]
         , relationships :: [Relationship]
         }
    deriving stock (Eq, Show)


data Value
  = B !Bool
  | F !Double
  | I !Integer -- could be Int64, but hedis uses Integer
  | L [Value]
  | N
  | P Path
  | R Relationship
  | T ByteString
  | V Node
  deriving stock (Eq, Show)

type Query = ByteString
type GraphName = ByteString
type Parameters = [(ByteString, Value)]

redisGraphQuery :: ByteString -> ByteString -> Redis (Either Reply ByteString)
redisGraphQuery graph q
  = Redis.sendRequest ["GRAPH.QUERY", graph, q, "--compact"]

-- query_ :: GraphName -> Query -> Redis ()
-- -- TODO: how best to check if there was an error?
-- query_ graph q = do
--   r <- query graph q
--   return ()

query :: GraphName -> Query -> Redis QueryResult
query graph q = cypherDecode <$> redisGraphQuery graph q

queryP :: GraphName -> Query -> Parameters -> Redis QueryResult
queryP g q ps
  | null ps = query g q
  | otherwise = query g q'
  where
    q' = encodeParameters ps <> q

-- queryP_ :: GraphName -> ByteString -> Parameters -> Redis ()
-- queryP_ g q ps
--   | null ps = query_ g q
--   | otherwise = query_ g q'
--   where
--     q' = encodeParameters ps <> q

encodeParameters :: Parameters -> ByteString
encodeParameters ps = "CYPHER " <> BC.unwords (fmap go ps) <> ";"
  where
    go (name, v) = name <> "=" <> encodeValue v
    encodeValue N{} = "null"
    encodeValue (B True) = "true"
    encodeValue (B False) = "false"
    encodeValue (I n) = BC.pack $ show n
    encodeValue (F n) = BC.pack $ show n
    encodeValue (L xs) = "[" <> B.intercalate "," (fmap encodeValue xs) <> "]"
    encodeValue (T t) = BC.pack $ show t -- easiest way to get string as literal 😢
    encodeValue V{} = encodingErr "node"
    encodeValue R{} = encodingErr "relationship"
    encodeValue P{} = encodingErr "path"
    encodingErr thing = error
      $ concat ["Encoding error: can not encode ", thing, "  as parameter"]


type Header = [ByteString]
type Record = [Value]
type QueryStats = [ByteString]
data QueryResult
  = QueryResult { header :: Header
                , rows :: [Record]
                , stats :: QueryStats
                }
    deriving stock (Show)

-- type IdMap = IntMap ByteString

-- updateIdMap :: GraphName -> Query -> IdMap -> Redis IdMap
-- updateIdMap g q m = do
--   let mmax = fst <$> IntMap.lookupMax m
--       param = ("lastId", I $ maybe 0 toInteger mmax)
--   _ <- queryP g q [param]
--   return _

-- TODO: procedure calls give us the string for the integer labels
cypherDecode :: Either Reply ByteString -> QueryResult
-- DOUBT: I'm not sure what kinds of values are returned by the Right
-- constructor…
--- uses 'error' a lot, since there's little to do but fail if the decoding goes
--- wrong (as per https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/);
--- if you have a better idea of how to handle this, do tell!
cypherDecode (Left (MultiBulk (Just topLevel)))
  = case topLevel of
      [stats] -> QueryResult [] [] (decodeStats stats)
      [header, results,stats]
        -> QueryResult (decodeHeader header) (decodeResults results) (decodeStats stats)
      _ -> decodingErr "Wrong top-level format"
  where
    decodeStats (MultiBulk (Just ss)) = go <$> ss
      where
        go (Bulk (Just s)) = s
        go _ = decodingErr "stat"
    decodeStats _ = decodingErr "stats"
    -- each result column is a 2-array whose first element may be ignored and
    -- the second is the label (string)
    decodeHeader header
      = case header of
          (MultiBulk (Just hs)) -> fmap toLabel hs
          _ -> decodingErr "header"
      where
        toLabel (MultiBulk (Just [_backwardsCompatibilityIgnore, Bulk (Just l)])) = l
        toLabel _ = decodingErr "column label"
    decodeResults :: Reply -> [[Value]]
    decodeResults (MultiBulk (Just rs)) = fmap decodeLine rs
      where
        decodeLine (MultiBulk (Just cs)) = fmap decodeValue cs
          where
            decodeValue (MultiBulk (Just (valueType:xs))) =
              (case valueType of
                Integer 1 -> const N -- null
                Integer 2 -> decodeStr -- string/text
                Integer 3 -> decodeInt -- integer
                Integer 4 -> decodeBool -- boolean
                Integer 5 -> decodeDouble -- double
                Integer 6 -> decodeList -- array
                Integer 7 -> R . decodeEdge -- edge
                Integer 8 -> V . decodeNode -- node
                Integer 9 -> decodePath -- path
                _ -> decodingErr "value type") xs
              where
                decodeInt [Integer n] = I n
                decodeInt _ = decodingErr "integer"
                decodeBool [Bulk (Just b)] = case b of "true" -> B True; _ -> B False
                decodeBool _ = decodingErr "boolean"
                decodeStr [Bulk (Just s)] = T s
                decodeStr _ = decodingErr "string"
                -- bad implementation
                decodeDouble [Bulk (Just d)] = F . read $ BC.unpack d
                decodeDouble _ = decodingErr "double"
                decodeList [MultiBulk (Just vs)] = L $ fmap decodeValue vs
                decodeList _ = decodingErr "list"
                decodeEdge [MultiBulk (Just [Integer relId, Integer relType, Integer srcId, Integer dstId, MultiBulk (Just props)])]
                  = Relationship relId relType srcId dstId (fmap decodeProperty props)
                decodeEdge _ = decodingErr "relationship"
                decodeNode [MultiBulk (Just [Integer nodeId, MultiBulk (Just labels), MultiBulk (Just properties)])]
                  = Node nodeId
                         (fmap decodeNodeLabel labels)
                         (fmap decodeProperty properties)
                  where
                    decodeNodeLabel (Integer n) = n
                    decodeNodeLabel _ = decodingErr "node label"
                decodeNode _ = decodingErr "node"
                decodePath [MultiBulk (Just
                                       [MultiBulk (Just (Integer 6 : [MultiBulk (Just ns)]))
                                       , MultiBulk (Just (Integer 6 : [MultiBulk (Just rels)]))])]
                  = P $ Path decodedNodes decodedRels
                  where
                    dropType (MultiBulk (Just (_:things))) = things
                    dropType _ = decodingErr "entity"
                    decodedNodes = decodeNode . dropType <$> ns
                    decodedRels = decodeEdge . dropType <$> rels
                decodePath _ = decodingErr "path"
                decodeProperty (MultiBulk (Just [Integer propId, propType, value]))
                  = (propId, ) . decodeValue $ MultiBulk $ Just (propType : [value])
                decodeProperty _ = decodingErr "property"
            decodeValue _ = decodingErr "value"
        decodeLine _ = decodingErr "line"
    decodeResults _ = decodingErr "results"
cypherDecode (Left (Error msg)) = error $ BC.unpack msg
cypherDecode r = decodingErr $ "top level\n  " ++ show r


decodingErr :: String -> a
decodingErr msg = error $ "Decoding error: unexpected format for " ++ msg
