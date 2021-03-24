# hedisgraph

[![GitHub CI](https://github.com/odanoburu/hedisgraph/workflows/CI/badge.svg)](https://github.com/odanoburu/hedisgraph/actions)
<!-- [![Hackage](https://img.shields.io/hackage/v/hedisgraph.svg?logo=haskell)](https://hackage.haskell.org/package/hedisgraph) -->
<!-- [![Stackage Lts](http://stackage.org/package/hedisgraph/badge/lts)](http://stackage.org/lts/package/hedisgraph) -->
<!-- [![Stackage Nightly](http://stackage.org/package/hedisgraph/badge/nightly)](http://stackage.org/nightly/package/hedisgraph) -->
<!-- [![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) -->


This library offers support for
[RedisGraph](https://oss.redislabs.com/redisgraph/). It is but a thin
wrapper over the `hedis` library's `sendRequest` function, with
additional decoding logic exclusive to RedisGraph (as described in its
[client specification](https://oss.redislabs.com/redisgraph/client_spec/)).

## Status

This library is very much a work-in-progress.

- Support for node, relationship, and path return values not
  ergonomic. RedisGraph returns labels, types, and property names as
  integers, and offers procedures that when called return the mappings
  from those integers to strings. I do yet know the best way of
  handling this decoding in an ergonomic way (and since I have no use
  for it currently that is unlikely to change for now), so PRs are
  welcome!
- I have tried to keep the same types used by `hedis` (`Integer`,
  `ByteString`), but I would like to have a compatibility layer for
  the types used by the `hasbolt` library (`Int`, `Text`), which also
  handles Cypher queries. (One day RedisGraph might be a drop-in
  in-memory replacement for Neo4j, and this compatibility would help
  making this transition smoother.)
- I'm unsure of the best way of handling errors, so for now I take a
  simple approach of using `error` for errors that *shouldn't* happen
  is taken, and everything else is handled by `hedis`.
- There has been no optimization effort as of yet.


## Additional goods

There is also a package attempting to provide compatibility with the
[`hasbolt`](https://hackage.haskell.org/package/hasbolt) library,
since both libraries deal with data stores using the Cypher graph
query language.
