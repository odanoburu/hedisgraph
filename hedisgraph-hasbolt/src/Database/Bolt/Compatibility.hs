module Database.Bolt.Compatibility (
  boltResult, boltValue,
  redisParams, redisQuery, redisResult, redisValue,
  toText, toByteString
  ) where

import qualified Database.Bolt as Bolt
import qualified Database.RedisGraph as RedisG
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding


toByteString :: Text -> ByteString
toByteString = encodeUtf8

toText :: ByteString -> Text
toText = decodeUtf8

redisQuery :: Text -> ByteString
redisQuery = toByteString

redisValue :: Bolt.Value -> RedisG.Value
redisValue Bolt.N{} = RedisG.N
redisValue (Bolt.B b) = RedisG.B b
redisValue (Bolt.I i) = RedisG.I (toInteger i)
redisValue (Bolt.F d) = RedisG.F d
redisValue (Bolt.L vs) = RedisG.L (redisValue <$> vs)
redisValue (Bolt.T t) = RedisG.T (toByteString t)
redisValue (Bolt.M _m) = error "RedisGraph does not support maps yet"
redisValue (Bolt.S _s) = error "Not implemented yet"

boltValue :: RedisG.Value -> Bolt.Value
boltValue RedisG.N = Bolt.N ()
boltValue (RedisG.B b) = Bolt.B b
boltValue (RedisG.I i) = Bolt.I (fromInteger i)
boltValue (RedisG.F d) = Bolt.F d
boltValue (RedisG.L vs) = Bolt.L (boltValue <$> vs)
boltValue (RedisG.T t) = Bolt.T (toText t)
boltValue _ = error "Not implemented yet"

redisParams :: Map Text Bolt.Value -> [(ByteString, RedisG.Value)]
redisParams ps = go <$> Map.toList ps
  where
    go (n, v) = (toByteString n, redisValue v)

redisResult :: [Bolt.Record] -> RedisG.QueryResult
redisResult [] = RedisG.QueryResult [] [] []
redisResult (r:rs)
  = RedisG.QueryResult (toByteString <$> Map.keys r) (redisRecord <$> r:rs) []
  where
    redisRecord m = redisValue <$> Map.elems m

boltResult :: RedisG.QueryResult -> [Bolt.Record]
boltResult RedisG.QueryResult{header, rows} = go <$> rows
  where
    header' = toText <$> header
    go r = Map.fromList $ zip header' (boltValue <$> r)
