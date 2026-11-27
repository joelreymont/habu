-- host.sql - a mapped store for src/lib/sparql-test.f: schema sparql_test,
-- its tables, the shapes `id`, `string`, `number` and `decimal`, the key set
-- `predicates`, and sparql_test.store, the store description. Node n is the
-- IRI urn:test:n; a number renders an integral value as xsd:integer and
-- others as canonical xsd:decimal; a decimal renders every value as
-- canonical xsd:decimal. The parts `class` and `thing` of `id`
-- hold node kinds and the subjects and objects of edge and value, which are
-- never a kind.
DROP SCHEMA IF EXISTS sparql_test CASCADE;
CREATE SCHEMA sparql_test;

CREATE TABLE sparql_test.node (id bigint PRIMARY KEY, label text, kind bigint);
CREATE TABLE sparql_test.edge (s bigint NOT NULL, p bigint NOT NULL, o bigint NOT NULL);
CREATE TABLE sparql_test.value (s bigint NOT NULL, p bigint NOT NULL, v numeric NOT NULL);
CREATE TABLE sparql_test.note (id bigint NOT NULL, note text NOT NULL);
CREATE TABLE sparql_test.mark (s bigint NOT NULL, o bigint NOT NULL);
CREATE TABLE sparql_test.fact (s bigint NOT NULL, p bigint NOT NULL, pid bigint NOT NULL,
    o bigint NOT NULL);
CREATE TABLE sparql_test.typed (s bigint NOT NULL, o_id bigint, o_text text, o_num numeric,
    o_kind bigint, CHECK (num_nonnulls(o_id, o_text, o_num, o_kind) <= 1));
CREATE TABLE sparql_test.held (id bigint PRIMARY KEY, s bigint NOT NULL, p bigint NOT NULL,
    pid bigint NOT NULL, o_num numeric, o_text text, alt bigint, CHECK (num_nonnulls(o_num, o_text) <= 1));

CREATE FUNCTION sparql_test.id(term jsonb) RETURNS bigint
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='uri' AND term->>'value' ~ '^urn:test:[1-9][0-9]{0,17}$'
                THEN substring(term->>'value' FROM 10)::bigint END
$$;

CREATE FUNCTION sparql_test.id_term(id bigint) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN id IS NOT NULL THEN jsonb_object(ARRAY['type','uri','value','urn:test:' || id::text]) END
$$;

CREATE FUNCTION sparql_test.string(term jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal' AND NOT term ? 'datatype' AND NOT term ? 'xml:lang'
                THEN term->>'value' END
$$;

CREATE FUNCTION sparql_test.string_term(value text) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN value IS NOT NULL THEN jsonb_object(ARRAY['type','literal','value',value]) END
$$;

-- Only the canonical lexical forms are in the shape, so `in` inverts `out`.
CREATE FUNCTION sparql_test.number(term jsonb) RETURNS numeric
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal'
                 AND (term->>'datatype'='http://www.w3.org/2001/XMLSchema#integer'
                      AND term->>'value' ~ '^(0|-?[1-9][0-9]*)$'
                   OR term->>'datatype'='http://www.w3.org/2001/XMLSchema#decimal'
                      AND term->>'value' ~ '^-?(0|[1-9][0-9]*)\.[0-9]*[1-9]$')
                THEN (term->>'value')::numeric END
$$;

CREATE FUNCTION sparql_test.number_term(value numeric) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN value=trunc(value)
        THEN jsonb_object(ARRAY['type','literal','value',trunc(value)::text,
                                'datatype','http://www.w3.org/2001/XMLSchema#integer'])
        WHEN value IS NOT NULL
        THEN jsonb_object(ARRAY['type','literal','value',trim_scale(value)::text,
                                'datatype','http://www.w3.org/2001/XMLSchema#decimal']) END
$$;

-- Only the canonical lexical forms are in the shape, so `in` inverts `out`.
CREATE FUNCTION sparql_test.decimal(term jsonb) RETURNS numeric
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal'
                 AND term->>'datatype'='http://www.w3.org/2001/XMLSchema#decimal'
                 AND term->>'value' ~ '^(-?(0|[1-9][0-9]*)\.[0-9]*[1-9]|-?[1-9][0-9]*\.0|0\.0)$'
                THEN (term->>'value')::numeric END
$$;

CREATE FUNCTION sparql_test.decimal_term(value numeric) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN value IS NOT NULL
        THEN jsonb_object(ARRAY['type','literal','value',
                                CASE WHEN value=trunc(value) THEN trunc(value)::text || '.0'
                                     ELSE trim_scale(value)::text END,
                                'datatype','http://www.w3.org/2001/XMLSchema#decimal']) END
$$;

CREATE FUNCTION sparql_test.store(bytes bigint DEFAULT 1048576) RETURNS jsonb
LANGUAGE sql IMMUTABLE STRICT AS $$
SELECT jsonb_build_object('bytes',bytes) || $store${
  "shapes": {
    "id": {"in": "sparql_test.id", "out": "sparql_test.id_term", "holds": "uri",
           "type": "bigint"},
    "class": {"within": "id"},
    "thing": {"within": "id"},
    "string": {"in": "sparql_test.string", "out": "sparql_test.string_term",
               "holds": ["http://www.w3.org/2001/XMLSchema#string"], "type": "text"},
    "number": {"in": "sparql_test.number", "out": "sparql_test.number_term",
               "holds": ["http://www.w3.org/2001/XMLSchema#integer",
                         "http://www.w3.org/2001/XMLSchema#decimal"],
               "type": "numeric"},
    "decimal": {"in": "sparql_test.decimal", "out": "sparql_test.decimal_term",
                "holds": ["http://www.w3.org/2001/XMLSchema#decimal"], "type": "numeric"}},
  "keys": {
    "predicates": {"type": "bigint[]",
      "values": {"urn:test:70": "{700,701}", "urn:test:71": "{710}"}}},
  "sources": [
    {"from": "sparql_test.node", "graph": null,
     "s": {"col": "id", "shape": "id", "unique": true},
     "p": {"term": {"type": "uri", "value": "http://www.w3.org/2000/01/rdf-schema#label"}},
     "o": {"col": "label", "shape": "string"}},
    {"from": "sparql_test.node", "graph": null,
     "s": {"col": "id", "shape": "id", "unique": true},
     "p": {"term": {"type": "uri", "value": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}},
     "o": {"col": "kind", "shape": "class"}},
    {"from": "sparql_test.edge", "graph": null,
     "s": {"col": "s", "shape": "thing"},
     "p": {"col": "p", "shape": "id"},
     "o": {"col": "o", "shape": "thing"}},
    {"from": "sparql_test.value", "graph": null,
     "s": {"col": "s", "shape": "thing"},
     "p": {"col": "p", "shape": "id"},
     "o": {"col": "v", "shape": "number"}},
    {"from": "sparql_test.note", "graph": {"type": "uri", "value": "urn:test:notes"},
     "s": {"col": "id", "shape": "id"},
     "p": {"term": {"type": "uri", "value": "http://www.w3.org/2000/01/rdf-schema#comment"}},
     "o": {"col": "note", "shape": "string"}},
    {"from": "sparql_test.mark", "graph": {"type": "uri", "value": "urn:test:7"},
     "s": {"col": "s", "shape": "id"},
     "p": {"term": {"type": "uri", "value": "urn:test:23"}},
     "o": {"col": "o", "shape": "id"}},
    {"from": "sparql_test.fact", "graph": null,
     "s": {"col": "s", "shape": "id"},
     "p": {"col": "p", "shape": "id", "key": {"col": "pid", "set": "predicates"}},
     "o": {"col": "o", "shape": "id"}},
    {"from": "sparql_test.typed", "graph": null,
     "s": {"col": "s", "shape": "id"},
     "p": {"term": {"type": "uri", "value": "urn:test:80"}},
     "o": {"cols": [{"col": "o_id", "shape": "thing"},
                    {"col": "o_text", "shape": "string"},
                    {"col": "o_num", "shape": "number"},
                    {"col": "o_kind", "shape": "class"}]}}]}$store$::jsonb
$$;

-- Nodes 1-4 are machines and parts, 10 and 11 their classes; node 5 has no
-- label; nodes 40-43, of class 12, have labels that order by code point.
-- Predicates: 20 part of, 21 next to, 22 same as, 23 marked, 25 the cycle
-- 50, 51, 52, 26 the chain 60, 61, 62, 30 weight, 31 count, 70 and 71 the
-- facts among 40-43, keyed 700 and 701 for 70 and 710 for 71, 80 the typed
-- objects of 40-43, a node, a label, a weight and a class the graph holds
-- already, and a row of 43's with none; 81 a number, 82 a text, 83 both and
-- 84 neither declared, the predicates of statements 801-808 about 40-43,
-- keyed 810, 820, 830 and 831, and 840. The named
-- graphs are urn:test:notes and urn:test:7, a node's IRI, so an id column
-- can hold its name.
INSERT INTO sparql_test.node VALUES
    (1,'Lathe',10),(2,'Mill',10),(3,'Spindle',11),(4,'Bolt',11),(5,NULL,11),
    (10,'Machine',NULL),(11,'Part',NULL),(20,'part of',NULL),(30,'weight',NULL),
    (40,'b',12),(41,'B',12),(42,'a',12),(43,'é',12);
INSERT INTO sparql_test.edge VALUES (3,20,1),(4,20,2),(5,20,1),(1,21,2),(4,22,4),
    (50,25,51),(51,25,52),(52,25,50),(60,26,61),(61,26,62);
INSERT INTO sparql_test.value VALUES (1,30,1200),(2,30,950.5),(3,30,12),(4,30,0.25),(1,31,2);
INSERT INTO sparql_test.note VALUES (1,'oiled'),(3,'worn');
INSERT INTO sparql_test.mark VALUES (1,7),(2,8);
INSERT INTO sparql_test.fact VALUES (40,70,700,41),(42,70,701,43),(43,71,710,40);
INSERT INTO sparql_test.typed VALUES (40,1,NULL,NULL,NULL),(41,NULL,'a',NULL,NULL),
    (42,NULL,NULL,0.25,NULL),(43,NULL,NULL,NULL,10),(43,NULL,NULL,NULL,NULL);
INSERT INTO sparql_test.held (id, s, p, pid, o_num, o_text) VALUES (801,40,81,810,3,NULL),(802,41,81,810,4.5,NULL),
    (803,40,82,820,NULL,'b'),(804,41,82,820,NULL,'a'),(805,40,83,830,7,NULL),
    (806,41,83,831,NULL,'c'),(807,42,84,840,2,NULL),(808,43,84,840,NULL,'d');
