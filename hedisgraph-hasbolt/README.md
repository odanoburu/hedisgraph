# hedisgraph-hasbolt

Package offering compatibility between `hedisgraph` and
[`hasbolt`](https://hackage.haskell.org/package/hasbolt) types.

## Why?

Since both RedisGraph and BOLT-protocol-using data stores like Neo4j
or Memgraph use Cypher as their query language, one might need to port
a project from one to the other, or to support all these Cypher-using
data stores.

## TODO

- [ ] Redis -> Bolt
- [x] Bolt -> Redis
