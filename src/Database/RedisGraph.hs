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
  ( Node(..), Path(..), Record, Redis, Relationship(..), QueryResult(..), Value(..), Parameters
  , query, queryP
  , runRedisGraph)
where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as Encoding
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Database.Redis (Redis, Reply(..))
import qualified Database.Redis as Redis
--import Debug.Trace (trace)

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
  | L ![Value]
  | M !(Map Text Value)
  | N
  | P !Path
  | R !Relationship
  | T !Text
  | V !Node
  deriving stock (Eq, Show)

type Query = Text
type GraphName = ByteString
type Parameters = [(Text, Value)]

redisGraphQuery :: ByteString -> ByteString -> Redis (Either Reply ByteString)
redisGraphQuery graph rawQuery
  = Redis.sendRequest ["GRAPH.QUERY", graph, rawQuery, "--compact"]

-- query_ :: GraphName -> Query -> Redis ()
-- -- TODO: how best to check if there was an error?
-- query_ graph q = do
--   r <- query graph q
--   return ()

query :: GraphName -> Query -> Redis QueryResult
query graph q = cypherDecode <$> redisGraphQuery graph (Encoding.encodeUtf8 q)

queryP :: GraphName -> Query -> Parameters -> Redis QueryResult
queryP g q ps
  | null ps = query g q
  -- NOTE: repeats 'query', but since it's short, I'm fine with it
  | otherwise = cypherDecode <$> redisGraphQuery g q'
  where
    q' = encodeParameters ps <> (Encoding.encodeUtf8 q)

encodeParameters :: Parameters -> ByteString
encodeParameters ps = "CYPHER " <> BC.unwords (fmap go ps) <> " "
  where
    go (name, v) = encodeEscapeName name <> "=" <> encodeValue v
    encodeEscapeName = Encoding.encodeUtf8 . escapeName
    escapeName name =
      if Text.all isAlphaNum name
      then name
      else -- needs escaping
        escaped
      where
        -- NOTE: could be made more complex if escaping is
        -- implemented
        -- https://github.com/RedisGraph/RedisGraph/issues/1187
        escaped = Text.cons '`' . (`Text.snoc` '`') $ Text.filter (/= '`') name
    encodeValue N{} = "null"
    encodeValue (B True) = "true"
    encodeValue (B False) = "false"
    encodeValue (I n) = BC.pack $ show n
    encodeValue (F n) = BC.pack $ show n
    encodeValue (L xs) = "[" <> B.intercalate "," (fmap encodeValue xs) <> "]"
    encodeValue (T t) = Encoding.encodeUtf8 . flip Text.snoc '"' . Text.cons '"' $ Text.concatMap encodeChars t
      where
        encodeChars = \case
          '"' -> "\\\""
          '\\' ->  "\\\\"
          c -> Text.singleton c
    encodeValue (M m) = "{" <> B.intercalate "," (fmap encodePair $ Map.toList m) <> "}"
      where
        encodePair (k, v) = encodeEscapeName k <> ":" <> encodeValue v
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
                Integer 0 -> error "Decoding error: unknown value type"
                Integer 1 -> const N -- null
                Integer 2 -> decodeStr -- string/text
                Integer 3 -> decodeInt -- integer
                Integer 4 -> decodeBool -- boolean
                Integer 5 -> decodeDouble -- double
                Integer 6 -> decodeList -- array
                Integer 7 -> R . decodeEdge -- edge
                Integer 8 -> V . decodeNode -- node
                Integer 9 -> decodePath -- path
                Integer 10 -> decodeMap -- map
                Integer 11 -> decodePoint -- TODO: point
                _ -> error "Decoding error: value type not suported yet") xs
              where
                decodeInt [Integer n] = I n
                decodeInt _ = decodingErr "integer"
                decodeBool [Bulk (Just b)] = case b of "true" -> B True; _ -> B False
                decodeBool _ = decodingErr "boolean"
                decodeStr [Bulk (Just s)] = T (Encoding.decodeUtf8 s)
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
                decodeMap [MultiBulk (Just vs)] = M $ decodePairs mempty vs
                  where
                    decodePairs :: Map Text Value -> [Reply] -> Map Text Value
                    decodePairs mapSoFar = \case
                      Bulk (Just k) : v : vs' -> decodePairs (Map.insert (Encoding.decodeUtf8 k) (decodeValue v) mapSoFar) vs'
                      [] -> mapSoFar
                      _ -> decodingErr "key-value pair"
                decodeMap _ = decodingErr "map"
                decodePoint _ = error "Decoding error: points not implemented yet"
            decodeValue _ = decodingErr "value"
        decodeLine _ = decodingErr "line"
    decodeResults _ = decodingErr "results"
cypherDecode (Left (Error msg)) = error $ BC.unpack msg
cypherDecode r = decodingErr $ "top level\n  " ++ show r


decodingErr :: String -> a
decodingErr msg = error $ "Decoding error: unexpected format for " ++ msg

runRedisGraph :: Redis.ConnectInfo -> Redis a -> IO a
runRedisGraph cInfo redis = do
  conn <- Redis.checkedConnect cInfo
  res <- Redis.runRedis conn redis
  Redis.disconnect conn
  return res
