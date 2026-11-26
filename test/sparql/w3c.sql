-- w3c.sql - a store for test/sparql/w3c-test.f: schema sparql_w3c, one W3C
-- case's dataset as quads of JSONB terms, and sparql_w3c.load, which replaces
-- the dataset and answers its store description. The one shape, `term`, is
-- the term itself; each graph is a source, the default graph's `graph` null.
DROP SCHEMA IF EXISTS sparql_w3c CASCADE;
CREATE SCHEMA sparql_w3c;

-- g is null for the default graph.
CREATE TABLE sparql_w3c.quad (
    s jsonb NOT NULL, p jsonb NOT NULL, o jsonb NOT NULL, g jsonb,
    UNIQUE NULLS NOT DISTINCT (s, p, o, g));

CREATE FUNCTION sparql_w3c.term(term jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$ SELECT term $$;

CREATE FUNCTION sparql_w3c.source(g jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE AS $$
SELECT jsonb_build_object(
    'from', format('(SELECT s, p, o FROM sparql_w3c.quad WHERE g %s)',
                   CASE WHEN g IS NULL THEN 'IS NULL'
                        ELSE format('= %L::jsonb', g::text) END),
    'graph', g,
    's', '{"col": "s", "shape": "term"}'::jsonb,
    'p', '{"col": "p", "shape": "term"}'::jsonb,
    'o', '{"col": "o", "shape": "term"}'::jsonb)
$$;

-- lines holds one JSON array [s,p,o,g] per line.
CREATE FUNCTION sparql_w3c.load(lines text) RETURNS jsonb
LANGUAGE sql AS $$
DELETE FROM sparql_w3c.quad;
INSERT INTO sparql_w3c.quad
SELECT q->0, q->1, q->2, NULLIF(q->3, 'null')
FROM regexp_split_to_table(lines, E'\n') AS l, LATERAL CAST(l AS jsonb) AS q
WHERE l <> '';
SELECT jsonb_build_object(
    'bytes', 16777216,
    'shapes', '{"term": {"in": "sparql_w3c.term", "out": "sparql_w3c.term"}}'::jsonb,
    'sources', jsonb_build_array(sparql_w3c.source(NULL))
        || coalesce((SELECT jsonb_agg(sparql_w3c.source(g) ORDER BY g)
                     FROM (SELECT DISTINCT g FROM sparql_w3c.quad WHERE g IS NOT NULL) AS d),
                    '[]'));
$$;

-- doc, a SPARQL Results document, with each literal of a numeric datatype
-- in one form per value, 0 for -0 too: the suite spells one double 3.21E4
-- in agg-sum-02 and 2100 in agg-sum-distinct. Every other term, and a form
-- that is not of its datatype's kind, stays as it is.
CREATE FUNCTION sparql_w3c.number_forms(doc jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE SET extra_float_digits = 1 AS $$
SELECT coalesce(jsonb_set(doc, '{results,bindings}', jsonb_agg(s ORDER BY n)), doc)
FROM jsonb_array_elements(doc #> '{results,bindings}') WITH ORDINALITY AS b(r, n),
LATERAL (SELECT coalesce(jsonb_object_agg(k, CASE WHEN v ~ CASE
        WHEN kind = 'integer' THEN '^[+-]?[0-9]+$'
        WHEN kind = 'decimal' THEN '^[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)$'
        WHEN kind IN ('float', 'double')
        THEN '^([+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee][+-]?[0-9]+)?|-?INF|NaN)$' END
    THEN jsonb_set(t, '{value}', to_jsonb(CASE kind
        WHEN 'double' THEN (v::float8 + 0)::text
        WHEN 'float' THEN (v::float4 + 0)::text
        ELSE trim_scale(v::numeric)::text END))
    ELSE t END), '{}') AS s
    FROM jsonb_each(r) AS e(k, t),
    LATERAL (SELECT t->>'value' AS v, sparql.numeric_kind(t->>'datatype') AS kind) l) x
$$;
