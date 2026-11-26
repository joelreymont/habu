# SPARQL

`src/lib/sparql.f` compiles a read-only SPARQL query to one parameterized
PostgreSQL statement over the store a host describes. Habu parses the query
and builds the SQL; PostgreSQL evaluates it. The library owns no connection
and no copy of the data: the host runs the statement in its own transaction
over its own tables and views.

## Entry

```forth
SPARQL:COMPILE ( query:str store:str sql:^VEC:vector<u8> params:[ str -- ] -- bool )
```

Its contract, what it appends and what the host must provide, is the header
of [src/lib/sparql.f](../src/lib/sparql.f). The store description's JSON
format is the header of [src/lib/sparql/store.f](../src/lib/sparql/store.f).
The host loads [src/lib/sparql/runtime.sql](../src/lib/sparql/runtime.sql)
into schema `sparql`; the statement calls its term functions.

## Queries

A query is a SELECT or an ASK. CONSTRUCT, DESCRIBE, updates, FROM and FROM
NAMED, SERVICE, BNODE() and SHA1 are rejected, each rejection naming the
rewrite.
