{-# LANGUAGE QuasiQuotes #-}
module Database.RedisGraphSpec where

import Database.Redis
import Database.RedisGraph
import Test.Hspec
import Data.String.Interpolate (i)

redis :: Redis QueryResult -> IO [Record]
redis action = do
  conn <- checkedConnect defaultConnectInfo
  r <- runRedis conn action
  disconnect conn
  return $ rows r

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Decoding tests" $ do
    it "returns null"
      $ redis (query "redisgraph-test" "RETURN null AS nothing")
      `shouldReturn` [[N]]

    it "returns true"
      $ redis (query "redisgraph-test" "RETURN true AS bool")
      `shouldReturn` [[B True]]

    it "returns positive integer"
      $ redis (query "redisgraph-test" "RETURN 42 AS integer")
      `shouldReturn` [[I 42]]

    it "returns negative integer"
      $ redis (query "redisgraph-test" "RETURN -9 AS integer")
      `shouldReturn` [[I (-9)]]

    it "returns large positive integer" -- integers are 64-bit
      $ redis (query "redisgraph-test" "RETURN 9223372036854775807 AS integer")
      `shouldReturn` [[I 9223372036854775807]]

    it "returns large negative integer"
      $ redis (query "redisgraph-test" "RETURN -9223372036854775807 AS integer")
      `shouldReturn` [[I (-9223372036854775807)]]

    it "returns text"
      $ redis (query "redisgraph-test" "RETURN 'iraçú-porã α🤔ఓ' AS text")
      `shouldReturn` [[T "iraçú-porã α🤔ఓ"]]

    it "returns float"
      $ redis (query "redisgraph-test" "RETURN 1.1415 AS float")
      `shouldReturn` [[F 1.1415]]

    it "returns list"
      $ redis (query "redisgraph-test"
               "RETURN [null, true, 1, -1, 'opa', 3.2, [null, true, 1, -1, 'opa', 3.2]] AS list")
      `shouldReturn` [[L [N, B True, I 1, I (-1), T "opa", F 3.2
                                  , L [N, B True, I 1, I (-1), T "opa", F 3.2]]]]

    it "returns node"
      $ redis (query "redisgraph-test"
               "CREATE (n:Test {i: 1}) RETURN n AS node")
      -- NOTE: there is no way of knowing what the node id will be, so we use a
      -- negative one mostly to check that there's no encoding error
      --- FIXME: could be made better by checking properties and labels, but
      --- before we add the decoding of their IDs they'll have the same problem
      --- as node (and relationship) IDs
      `shouldNotReturn` [[V (Node (-1) [0] [(0, I 1)])]]

    it "returns relationship"
      $ redis (query "redisgraph-test"
               "CREATE (n:Test {i: 1})-[r:Test {j: true}]->(m:Test) RETURN r AS relationship")
      `shouldNotReturn` [[R (Relationship (-1) 0 0 0 [(1, B True)])]]

    it "returns path"
      $ redis (query "redisgraph-test"
               [i|CREATE (n:Test {i: 1})-[r:Test {j: true}]->(m:Test)
                 WITH true AS ignore
                 MATCH p = (m)-->(n)
                 RETURN p AS path|])
      `shouldNotReturn` [[P (Path {nodes = [], relationships = []})]]
