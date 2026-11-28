-- runtime.sql - schema sparql: the RDF term functions compiled SPARQL statements
-- call. A term is a SPARQL Results JSON object; SQL NULL is an expression error.
-- Query cancellation, resource failures and database faults are never turned
-- into an unbound term. A computed number is spelled as XPath 3.1 casts it to
-- xs:string, through format_double and format_decimal, the form STR() answers;
-- a selected RDF term keeps its original lexical form.
-- A reinstall drops these aggregates, functions and composite types by name,
-- never by CASCADE: a host object that still uses one makes the install fail
-- instead of vanishing with it.
-- The IMMUTABLE SQL functions inline into the statement that calls them. So
-- their bodies call only immutable functions and are not STRICT, a NULL
-- argument giving NULL through a CASE.
DO $$
DECLARE names text;
BEGIN
    SELECT string_agg(format('sparql.%I(%s)', p.proname, pg_get_function_identity_arguments(p.oid)), ', ') INTO names
      FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
     WHERE n.nspname = 'sparql' AND p.prokind = 'a';
    IF names IS NOT NULL THEN EXECUTE 'DROP AGGREGATE ' || names; END IF;
    SELECT string_agg(format('sparql.%I(%s)', p.proname, pg_get_function_identity_arguments(p.oid)), ', ') INTO names
      FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
     WHERE n.nspname = 'sparql';
    IF names IS NOT NULL THEN EXECUTE 'DROP FUNCTION ' || names; END IF;
    SELECT string_agg(format('sparql.%I', t.typname), ', ') INTO names
      FROM pg_type t JOIN pg_namespace n ON n.oid = t.typnamespace
     WHERE n.nspname = 'sparql' AND t.typtype = 'c';
    IF names IS NOT NULL THEN EXECUTE 'DROP TYPE ' || names; END IF;
END $$;
DROP SCHEMA IF EXISTS sparql;
CREATE SCHEMA sparql;

-- jsonb_object is IMMUTABLE where jsonb_build_object is STABLE, so this inlines.
CREATE FUNCTION sparql.term(value text, kind text DEFAULT 'literal', datatype text DEFAULT NULL)
RETURNS jsonb LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN value IS NULL THEN NULL
                WHEN datatype IS NULL OR datatype='http://www.w3.org/2001/XMLSchema#string'
                THEN jsonb_object(ARRAY['type',kind,'value',value])
                ELSE jsonb_object(ARRAY['type',kind,'value',value,'datatype',datatype]) END
$$;

CREATE FUNCTION sparql.normalize(term jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type' = 'literal' AND term ? 'xml:lang'
                THEN term - 'datatype' || jsonb_object(ARRAY['xml:lang',lower((term->>'xml:lang') COLLATE "C")])
                WHEN term->>'type' = 'literal'
                     AND term->>'datatype' = 'http://www.w3.org/2001/XMLSchema#string'
                THEN term - 'datatype' ELSE term END
$$;

-- A value without a zone is UTC's, the implicit timezone. Years are 0001 to
-- 294276, PostgreSQL's range, and fractions past a microsecond are cut. One out
-- of range is no instant; pg_input_is_valid tells without a subtransaction.
CREATE FUNCTION sparql.instant(value text,datatype text) RETURNS timestamptz
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    IF (strpos(value,'T')>0)<>(datatype='http://www.w3.org/2001/XMLSchema#dateTime') THEN RETURN NULL; END IF;
    IF value !~ '^([1-9][0-9]*)?[0-9]{4}-[0-9]{2}-[0-9]{2}(T[0-9]{2}:[0-9]{2}:[0-5][0-9](\.[0-9]+)?)?(Z|[+-](0[0-9]|1[0-3]):[0-5][0-9]|[+-]14:00)?$' THEN RETURN NULL; END IF;
    IF substr(split_part(value,'.',2),7,1) NOT IN ('','Z','+','-') THEN
        -- XSD 1.1: the end of a day, 24:00:00, has no fraction but zeros.
        IF value ~ 'T24:00:00\.0*[1-9]' THEN RETURN NULL; END IF;
        value := regexp_replace(value,'(\.[0-9]{6})[0-9]+','\1');
    END IF;
    -- PostgreSQL reads no date with a zone west of UTC.
    IF value LIKE '%-__-__-__:__' THEN value := left(value,-6) || 'T00:00:00' || right(value,6); END IF;
    value := value || CASE WHEN value LIKE '%Z' OR value LIKE '%+__:__' OR value LIKE '%-__:__' THEN '' ELSE 'Z' END;
    RETURN CASE WHEN pg_input_is_valid(value,'timestamptz') THEN value::timestamptz END;
END $$;

CREATE FUNCTION sparql.boolean(value boolean) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE value
        WHEN true THEN '{"type":"literal","value":"true","datatype":"http://www.w3.org/2001/XMLSchema#boolean"}'::jsonb
        WHEN false THEN '{"type":"literal","value":"false","datatype":"http://www.w3.org/2001/XMLSchema#boolean"}'::jsonb END
$$;

-- XPath 3.1 section 19.1.2.2, casting xs:double to xs:string: 0, -0, INF, -INF,
-- NaN; a magnitude in [1e-6, 1e6) as the decimal of the shortest text that
-- reads back exactly; otherwise XSD 1.1's canonical form: one nonzero digit
-- before the point, at least one after and no trailing zero beyond it, then E
-- and an exponent without + or leading zeros. An xs:float is passed as the
-- double its own shortest text reads as, so it keeps a float's digits and
-- falls on the same side of each bound as the float. A positive
-- extra_float_digits makes the text the shortest that reads back exactly.
CREATE FUNCTION sparql.format_double(value double precision) RETURNS text
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE SET extra_float_digits=1 AS $$
DECLARE lexical text := lower(value::text); sign text := ''; digits text; exponent integer;
BEGIN
    IF value='Infinity' THEN RETURN 'INF'; END IF;
    IF value='-Infinity' THEN RETURN '-INF'; END IF;
    IF value='NaN' THEN RETURN 'NaN'; END IF;
    IF value=0 THEN RETURN CASE WHEN left(lexical,1)='-' THEN '-0' ELSE '0' END; END IF;
    IF abs(value)>=0.000001 AND abs(value)<1000000 THEN RETURN sparql.format_decimal(lexical::numeric); END IF;
    IF left(lexical,1)='-' THEN sign := '-'; lexical := substr(lexical,2); END IF;
    exponent := coalesce(nullif(split_part(lexical,'e',2),''),'0')::integer;
    lexical := split_part(lexical,'e',1);
    exponent := exponent + length(split_part(lexical,'.',1)) - 1;
    digits := replace(lexical,'.','');
    exponent := exponent - length(digits) + length(ltrim(digits,'0'));
    digits := btrim(digits,'0');
    RETURN sign || left(digits,1) || '.' || coalesce(nullif(substr(digits,2),''),'0') || 'E' || exponent::text;
END $$;

-- XPath 3.1 section 19.1.2.2, casting xs:decimal to xs:string: an integral
-- value without a decimal point, otherwise no trailing zero. A nonfinite value
-- has none: NULL.
CREATE FUNCTION sparql.format_decimal(value numeric) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN min_scale(value) IS NOT NULL THEN trim_scale(value)::text END
$$;

-- A result's byte count, or SQLSTATE 54000 once it is past the byte limit.
CREATE FUNCTION sparql.within(total bigint, bytes bigint) RETURNS bigint
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    IF total>bytes THEN
        RAISE EXCEPTION 'SPARQL result exceeds % bytes',bytes USING ERRCODE='54000';
    END IF;
    RETURN total;
END $$;

-- The value of an xsd:float or xsd:double lexical form. One past its datatype's
-- range is INF, -INF or a zero of its sign (XSD 1.1 part 2, floatingPointRound):
-- the form is too large when its first nonzero digit, moved by the exponent,
-- stands before the point. pg_input_is_valid tells a form the cast cannot read
-- without a subtransaction. The length bound is numeric's, and keeps the
-- exponent within one.
CREATE FUNCTION sparql.float_value(value text, kind text) RETURNS double precision
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE mantissa text; whole text;
BEGIN
    IF value IN ('INF','+INF','-INF','NaN') THEN RETURN replace(value,'INF','Infinity')::double precision; END IF;
    IF octet_length(value)>16383 THEN RETURN NULL; END IF;
    -- Of these characters the cast reads only the decimal form, and all of it.
    IF value !~ '[^0-9.eE+-]' AND (CASE kind WHEN 'float' THEN pg_input_is_valid(value,'real')
                                             ELSE pg_input_is_valid(value,'double precision') END) THEN
        RETURN CASE kind WHEN 'float' THEN value::real ELSE value::double precision END;
    END IF;
    IF value !~ '^[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?$' THEN RETURN NULL; END IF;
    mantissa := ltrim(split_part(lower(value),'e',1),'+-');
    whole := ltrim(split_part(mantissa,'.',1),'0');
    RETURN (CASE WHEN starts_with(value,'-') THEN '-' ELSE '' END
            || CASE WHEN coalesce(nullif(split_part(lower(value),'e',2),''),'0')::numeric
                         + CASE WHEN whole<>'' THEN length(whole)
                                ELSE length(ltrim(split_part(mantissa,'.',2),'0'))-length(split_part(mantissa,'.',2)) END>0
                    THEN 'Infinity' ELSE '0' END)::double precision;
END $$;

CREATE FUNCTION sparql.float_numeric(value text, kind text) RETURNS numeric
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE SET extra_float_digits=3 AS $$
BEGIN
    RETURN sparql.float_value(value,kind)::text::numeric;
END $$;

-- A lexical form longer than 16,383 bytes, the most digits a numeric holds
-- after its point, is no number: so neither a cast below nor SUM can raise.
-- One CASE on the datatype, which PostgreSQL inlines into its caller.
CREATE FUNCTION sparql.numeric(term jsonb) RETURNS numeric
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE term->>'datatype'
        WHEN 'http://www.w3.org/2001/XMLSchema#integer' THEN
            CASE WHEN octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND term->>'type'='literal' THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#decimal' THEN
            CASE WHEN octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)$'
                      AND term->>'type'='literal' THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#double' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE THEN sparql.float_numeric(term->>'value','double') END
        WHEN 'http://www.w3.org/2001/XMLSchema#float' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE THEN sparql.float_numeric(term->>'value','float') END
        WHEN 'http://www.w3.org/2001/XMLSchema#nonPositiveInteger' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric<=0 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#negativeInteger' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric<0 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric>=0 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#positiveInteger' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric>0 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#long' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN -9223372036854775808 AND 9223372036854775807 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#int' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN -2147483648 AND 2147483647 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#short' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN -32768 AND 32767 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#byte' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN -128 AND 127 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#unsignedLong' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN 0 AND 18446744073709551615 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#unsignedInt' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN 0 AND 4294967295 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#unsignedShort' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN 0 AND 65535 THEN (term->>'value')::numeric END
        WHEN 'http://www.w3.org/2001/XMLSchema#unsignedByte' THEN
            CASE WHEN (term->>'type'<>'literal') IS NOT TRUE AND octet_length(term->>'value')<=16383 AND term->>'value' ~ '^[+-]?[0-9]+$'
                      AND (term->>'value')::numeric BETWEEN 0 AND 255 THEN (term->>'value')::numeric END
    END
$$;

CREATE FUNCTION sparql.numeric_kind(datatype text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN datatype IS NULL THEN NULL
                WHEN datatype='http://www.w3.org/2001/XMLSchema#decimal' THEN 'decimal'
                WHEN datatype='http://www.w3.org/2001/XMLSchema#double' THEN 'double'
                WHEN datatype='http://www.w3.org/2001/XMLSchema#float' THEN 'float'
                WHEN datatype IN (
                    'http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#nonPositiveInteger',
                    'http://www.w3.org/2001/XMLSchema#negativeInteger', 'http://www.w3.org/2001/XMLSchema#long',
                    'http://www.w3.org/2001/XMLSchema#int', 'http://www.w3.org/2001/XMLSchema#short',
                    'http://www.w3.org/2001/XMLSchema#byte', 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
                    'http://www.w3.org/2001/XMLSchema#unsignedLong', 'http://www.w3.org/2001/XMLSchema#unsignedInt',
                    'http://www.w3.org/2001/XMLSchema#unsignedShort', 'http://www.w3.org/2001/XMLSchema#unsignedByte',
                    'http://www.w3.org/2001/XMLSchema#positiveInteger')
                THEN 'integer' END
$$;

CREATE FUNCTION sparql.nan(term jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT term->>'value'='NaN' AND term->>'type'='literal'
       AND term->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
$$;

-- A float goes through its own shortest text, so it is not spelled with a
-- double's digits.
CREATE FUNCTION sparql.float_number(value numeric, datatype text) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE SET extra_float_digits=1 AS $$
DECLARE lexical text := sparql.format_double(CASE WHEN datatype='http://www.w3.org/2001/XMLSchema#float'
                            THEN (value::real)::text::double precision ELSE value::double precision END);
BEGIN
    RETURN sparql.term(lexical,'literal',datatype);
END $$;

-- A computed number of a datatype, by its local name or its IRI.
CREATE FUNCTION sparql.number(value numeric, datatype text) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE lexical text; iri text := CASE WHEN starts_with(datatype,'http:') THEN datatype
                                       ELSE 'http://www.w3.org/2001/XMLSchema#' || datatype END;
BEGIN
    IF iri IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double') THEN
        RETURN sparql.float_number(value,iri);
    END IF;
    lexical := sparql.format_decimal(value);
    RETURN sparql.term(lexical,'literal',iri);
END $$;

-- Promote native floating inputs without losing signed zero or float precision.
-- A number past the width's range is an infinity or a zero of its sign; a
-- numeric casts through its text, which pg_input_is_valid tests.
CREATE FUNCTION sparql.floating(term jsonb,kind text) RETURNS double precision
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE source text := sparql.numeric_kind(term->>'datatype'); result double precision; value numeric;
BEGIN
    IF source IS NULL OR term->>'type'<>'literal' THEN RETURN NULL; END IF;
    IF source IN ('float','double') THEN
        result := sparql.float_value(term->>'value',source);
        IF source='float' OR kind<>'float' THEN RETURN CASE kind WHEN 'float' THEN result::real WHEN 'double' THEN result END; END IF;
        BEGIN
            RETURN result::real;
        EXCEPTION WHEN numeric_value_out_of_range THEN
            RETURN sign(sparql.numeric(term))::double precision
                 * CASE WHEN abs(sparql.numeric(term))<1 THEN 0::double precision ELSE 'Infinity' END;
        END;
    END IF;
    value := sparql.numeric(term);
    IF (CASE kind WHEN 'float' THEN pg_input_is_valid(value::text,'real')
                  WHEN 'double' THEN pg_input_is_valid(value::text,'double precision') END) IS FALSE THEN
        RETURN sign(value)::double precision * CASE WHEN abs(value)<1 THEN 0::double precision ELSE 'Infinity' END;
    END IF;
    RETURN CASE kind WHEN 'float' THEN value::real WHEN 'double' THEN value::double precision END;
END $$;

CREATE FUNCTION sparql.ebv(term jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE kind text := term->>'datatype'; value numeric;
BEGIN
    IF term->>'type'<>'literal' THEN RETURN NULL; END IF;
    IF kind='http://www.w3.org/2001/XMLSchema#boolean' THEN
        RETURN term->>'value' IN ('true','1');
    END IF;
    IF kind IS NULL OR kind IN ('http://www.w3.org/2001/XMLSchema#string',
                               'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') THEN
        RETURN length(term->>'value')>0;
    END IF;
    IF sparql.numeric_kind(kind) IS NULL THEN RETURN NULL; END IF;
    value := sparql.numeric(term);
    RETURN coalesce(value<>0 AND value<>'NaN'::numeric,false);
END $$;

-- One CASE, which PostgreSQL inlines into its caller. A float or double literal
-- is its own value at either width, and NaN only as the lexical form NaN.
CREATE FUNCTION sparql.compare(a jsonb,b jsonb) RETURNS integer
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE
        WHEN a IS NULL OR b IS NULL THEN NULL
        WHEN sparql.numeric_kind(a->>'datatype') IS NULL OR sparql.numeric_kind(b->>'datatype') IS NULL THEN
            CASE WHEN a->>'type'<>'literal' OR b->>'type'<>'literal' THEN NULL
                 WHEN coalesce(a->>'datatype','http://www.w3.org/2001/XMLSchema#string')='http://www.w3.org/2001/XMLSchema#string'
                      AND coalesce(b->>'datatype','http://www.w3.org/2001/XMLSchema#string')='http://www.w3.org/2001/XMLSchema#string' THEN
                     CASE WHEN lower(coalesce(a->>'xml:lang','') COLLATE "C")<>lower(coalesce(b->>'xml:lang','') COLLATE "C") THEN NULL
                          WHEN (a->>'value') COLLATE "C"<(b->>'value') COLLATE "C" THEN -1
                          WHEN (a->>'value') COLLATE "C">(b->>'value') COLLATE "C" THEN 1 ELSE 0 END
                 WHEN a->>'datatype' IS DISTINCT FROM b->>'datatype' THEN NULL
                 WHEN a->>'datatype'='http://www.w3.org/2001/XMLSchema#boolean'
                      AND a->>'value' IN ('true','false','1','0') AND b->>'value' IN ('true','false','1','0') THEN
                     (a->>'value' IN ('true','1'))::integer-(b->>'value' IN ('true','1'))::integer
                 WHEN a->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#dateTime','http://www.w3.org/2001/XMLSchema#date') THEN
                     timestamptz_cmp(sparql.instant(a->>'value',a->>'datatype'),sparql.instant(b->>'value',b->>'datatype'))
            END
        WHEN a->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
             OR b->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double') THEN
            CASE WHEN a->>'value'='NaN' AND a->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
                      OR b->>'value'='NaN' AND b->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
                 THEN NULL
                 ELSE btfloat8cmp(
                     CASE WHEN a->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
                               AND (a->>'type'<>'literal') IS NOT TRUE
                          THEN sparql.float_value(a->>'value',CASE a->>'datatype' WHEN 'http://www.w3.org/2001/XMLSchema#float' THEN 'float' ELSE 'double' END)
                          ELSE sparql.floating(a,CASE b->>'datatype' WHEN 'http://www.w3.org/2001/XMLSchema#double' THEN 'double' ELSE 'float' END) END,
                     CASE WHEN b->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
                               AND (b->>'type'<>'literal') IS NOT TRUE
                          THEN sparql.float_value(b->>'value',CASE b->>'datatype' WHEN 'http://www.w3.org/2001/XMLSchema#float' THEN 'float' ELSE 'double' END)
                          ELSE sparql.floating(b,CASE a->>'datatype' WHEN 'http://www.w3.org/2001/XMLSchema#double' THEN 'double' ELSE 'float' END) END) END
        ELSE numeric_cmp(sparql.numeric(a),sparql.numeric(b))
    END
$$;

CREATE FUNCTION sparql.equal(a jsonb,b jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE
        WHEN a=b THEN sparql.nan(a) IS NOT TRUE
        WHEN a->>'type'<>'literal' OR b->>'type'<>'literal' THEN false
        WHEN NOT a ? 'datatype' AND NOT b ? 'datatype' THEN
            a->>'value'=b->>'value' AND lower(coalesce(a->>'xml:lang','') COLLATE "C")=lower(coalesce(b->>'xml:lang','') COLLATE "C")
        WHEN a->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#integer','http://www.w3.org/2001/XMLSchema#decimal')
             AND b->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#integer','http://www.w3.org/2001/XMLSchema#decimal') THEN
            sparql.numeric(a)=sparql.numeric(b)
        -- RDF term identity still holds for an ill-typed literal, but never for NaN.
        ELSE coalesce(sparql.compare(a,b)=0,
                      CASE WHEN sparql.nan(a) OR sparql.nan(b) THEN false
                           WHEN sparql.normalize(a)=sparql.normalize(b) THEN true
                           WHEN a ? 'xml:lang' OR b ? 'xml:lang' THEN false END)
    END;
END $$;

CREATE FUNCTION sparql.relation(op text,a jsonb,b jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE ordering integer;
BEGIN
    ordering := sparql.compare(a,b);
    IF ordering IS NULL AND (sparql.nan(a) OR sparql.nan(b)) THEN RETURN false; END IF;
    RETURN CASE op WHEN '<' THEN ordering<0 WHEN '<=' THEN ordering<=0
                   WHEN '>' THEN ordering>0 WHEN '>=' THEN ordering>=0 END;
END $$;

CREATE FUNCTION sparql.float_arithmetic(op text,a jsonb,b jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE SET extra_float_digits=3 AS $$
DECLARE kind text; af double precision; bf double precision; result double precision;
BEGIN
    IF op IN ('u+','u-') THEN
        kind := sparql.numeric_kind(b->>'datatype');
        bf := sparql.floating(b,kind);
        IF op='u-' THEN bf := -bf; END IF;
        IF kind='float' THEN bf := (bf::real)::text::double precision; END IF;
        RETURN sparql.term(sparql.format_double(bf),'literal',b->>'datatype');
    END IF;
    kind := CASE WHEN a->>'datatype' LIKE '%#double' OR b->>'datatype' LIKE '%#double' THEN 'double' ELSE 'float' END;
    af:=sparql.floating(a,kind); bf:=sparql.floating(b,kind);
    IF af IS NULL OR bf IS NULL THEN RETURN NULL; END IF;
    IF op='/' AND bf=0 THEN
        result := CASE WHEN af=0 OR af='NaN'::double precision THEN 'NaN'
            WHEN (get_byte(float8send(af),0)&128)<>(get_byte(float8send(bf),0)&128) THEN '-Infinity'
            ELSE 'Infinity' END::double precision;
    ELSIF kind='float' THEN
        result := (CASE op WHEN '+' THEN af::real+bf::real WHEN '-' THEN af::real-bf::real
                           WHEN '*' THEN af::real*bf::real WHEN '/' THEN af::real/bf::real END)::text::double precision;
    ELSE
        result := CASE op WHEN '+' THEN af+bf WHEN '-' THEN af-bf WHEN '*' THEN af*bf WHEN '/' THEN af/bf END;
    END IF;
    RETURN sparql.term(sparql.format_double(result),'literal','http://www.w3.org/2001/XMLSchema#' || kind);
EXCEPTION WHEN numeric_value_out_of_range THEN RETURN NULL;
END $$;

-- No result of two exact operands is past a numeric's range: sparql.numeric
-- reads at most 16,383 digits.
CREATE FUNCTION sparql.arithmetic(op text,a jsonb,b jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE akind text := a->>'datatype'; bkind text := b->>'datatype'; av numeric; bv numeric;
BEGIN
    IF akind IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double')
       OR bkind IN ('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#double') THEN
        RETURN sparql.float_arithmetic(op,a,b);
    END IF;
    av := sparql.numeric(a); bv := sparql.numeric(b);
    av := CASE op WHEN '+' THEN av+bv WHEN '-' THEN av-bv WHEN '*' THEN av*bv
                  WHEN '/' THEN av/nullif(bv,0) WHEN 'u-' THEN -bv WHEN 'u+' THEN bv END;
    RETURN sparql.number(av,CASE WHEN op='/' OR akind='http://www.w3.org/2001/XMLSchema#decimal'
                                   OR bkind='http://www.w3.org/2001/XMLSchema#decimal' THEN 'decimal' ELSE 'integer' END);
END $$;

-- A term's ORDER BY key (SPARQL 1.1 section 15.1), separate from expression
-- comparison: unbound, blank, IRI, then literals: numbers by value, booleans,
-- dates by instant, then the rest by lexical form, language and datatype. The
-- caller keeps the selected original RDF term. A record compares its fields in
-- order, so text compares by code point whatever the database's collation.
CREATE TYPE sparql.order_key AS (kind integer, value numeric, lexical text COLLATE "C",
                                 language text COLLATE "C", datatype text COLLATE "C");

CREATE FUNCTION sparql.order(term jsonb) RETURNS sparql.order_key
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE value numeric := sparql.numeric(term);
BEGIN
    RETURN CASE
        WHEN term IS NULL THEN ROW(0,NULL,NULL,NULL,NULL)::sparql.order_key
        WHEN term->>'type'='bnode' THEN ROW(1,NULL,term->>'value',NULL,NULL)::sparql.order_key
        WHEN term->>'type'='uri' THEN ROW(2,NULL,term->>'value',NULL,NULL)::sparql.order_key
        WHEN term->>'datatype' IS NULL THEN ROW(6,NULL,term->>'value',lower((term->>'xml:lang') COLLATE "C"),NULL)::sparql.order_key
        WHEN term->>'datatype'='http://www.w3.org/2001/XMLSchema#boolean' THEN
            ROW(4,sparql.ebv(term)::integer,term->>'value',NULL,term->>'datatype')::sparql.order_key
        WHEN term->>'datatype' IN ('http://www.w3.org/2001/XMLSchema#dateTime','http://www.w3.org/2001/XMLSchema#date') THEN
            ROW(5,extract(epoch FROM sparql.instant(term->>'value',term->>'datatype')),term->>'value',NULL,term->>'datatype')::sparql.order_key
        WHEN value IS NOT NULL THEN ROW(3,value,term->>'value',NULL,term->>'datatype')::sparql.order_key
        ELSE ROW(6,NULL,term->>'value',lower((term->>'xml:lang') COLLATE "C"),term->>'datatype')::sparql.order_key
    END;
END $$;

-- The seven SPARQL constructor datatypes and numeric derived types. A bad
-- cast is an expression error; it must not cancel the surrounding query.
CREATE FUNCTION sparql.cast(datatype text,term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE SET extra_float_digits=3 AS $$
DECLARE x constant text := 'http://www.w3.org/2001/XMLSchema#';
    a jsonb := sparql.normalize(term); value text := a->>'value';
    source text := coalesce(a->>'datatype',x || 'string');
    target text := substring(datatype FROM length(x)+1); kind text := sparql.numeric_kind(datatype);
    n numeric; flag boolean; result jsonb; floating double precision;
BEGIN
    IF NOT starts_with(datatype,x) OR a ? 'xml:lang' THEN RETURN NULL; END IF;
    IF target='string' AND a->>'type'='uri' THEN RETURN sparql.term(value); END IF;
    IF a->>'type'<>'literal' THEN RETURN NULL; END IF;
    IF source=x || 'boolean' THEN
        flag := CASE value WHEN 'true' THEN true WHEN '1' THEN true WHEN 'false' THEN false WHEN '0' THEN false END;
        IF flag IS NULL THEN RETURN NULL; END IF;
        n := CASE WHEN flag THEN 1 ELSE 0 END;
    ELSIF sparql.numeric_kind(source) IS NOT NULL THEN
        n := sparql.numeric(a); IF n IS NULL THEN RETURN NULL; END IF;
    ELSIF source IN (x || 'dateTime',x || 'date') THEN
        IF sparql.instant(value,source) IS NULL THEN RETURN NULL; END IF;
    ELSIF source<>x || 'string' THEN RETURN NULL;
    END IF;
    IF target='string' THEN
        IF flag IS NOT NULL THEN RETURN sparql.term(flag::text); END IF;
        IF n IS NULL THEN RETURN sparql.term(value); END IF;
        RETURN sparql.term(CASE sparql.numeric_kind(source)
            WHEN 'double' THEN sparql.format_double(sparql.floating(a,'double'))
            WHEN 'float' THEN sparql.format_double((sparql.floating(a,'float')::real)::text::double precision)
            ELSE sparql.format_decimal(n) END);
    END IF;
    IF source=x || 'string' THEN value:=btrim(value,E' \t\r\n'); END IF;
    IF target='boolean' THEN
        RETURN sparql.boolean(CASE WHEN source=x || 'string' THEN
            CASE value WHEN 'true' THEN true WHEN '1' THEN true WHEN 'false' THEN false WHEN '0' THEN false END
            WHEN n IS NOT NULL THEN n<>0 AND n<>'NaN'::numeric END);
    END IF;
    IF target IN ('dateTime','date') THEN
        IF source NOT IN (x || 'string',datatype) OR sparql.instant(value,datatype) IS NULL THEN RETURN NULL; END IF;
        RETURN sparql.term(value,'literal',datatype);
    END IF;
    IF kind IS NULL THEN RETURN NULL; END IF;
    IF source=x || 'string' THEN n:=sparql.numeric(sparql.term(value,'literal',datatype)); END IF;
    IF n IS NULL OR kind IN ('integer','decimal') AND n IN ('Infinity'::numeric,'-Infinity'::numeric,'NaN'::numeric) THEN RETURN NULL; END IF;
    IF kind IN ('float','double') THEN
        floating:=sparql.floating(CASE WHEN source=x || 'string' THEN sparql.term(value,'literal',datatype)
            WHEN flag IS NOT NULL THEN sparql.number(n,'integer') ELSE a END,kind);
        IF kind='float' THEN floating:=(floating::real)::text::double precision; END IF;
        RETURN sparql.term(sparql.format_double(floating),'literal',datatype);
    END IF;
    IF kind='integer' THEN n:=trunc(n); END IF;
    result:=sparql.number(n,datatype);
    n:=sparql.numeric(result);
    RETURN CASE WHEN n IS NOT NULL THEN sparql.number(n,datatype) END;
EXCEPTION WHEN invalid_text_representation OR numeric_value_out_of_range THEN RETURN NULL;
END $$;

-- RFC 3986 section 5.2, including dot segments, query and fragment merging.
-- An existing IRI is unchanged; string arguments use the query's base. A
-- resolved IRI has at most the reference's and the base's bytes and one; it is
-- built, then raises SQLSTATE 54000 once it passes the byte limit.
CREATE FUNCTION sparql.iri(bytes bigint,term jsonb,base text) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE a jsonb:=sparql.normalize(term); value text:=a->>'value'; r text[]; b text[];
    pattern constant text:='^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$';
    scheme text; authority text; path text; query text; parts text[]; part text; first integer:=1; depth integer:=0;
    result text;
BEGIN
    IF a->>'type'='uri' THEN RETURN a; END IF;
    IF a->>'type' IS DISTINCT FROM 'literal' OR a ? 'datatype' OR a ? 'xml:lang'
       OR value ~ '[[:cntrl:] <>"{}|^`\\]' OR value ~ '%($|[^0-9A-Fa-f]|[0-9A-Fa-f]($|[^0-9A-Fa-f]))' THEN RETURN NULL; END IF;
    r:=regexp_match(value,pattern); b:=regexp_match(base,pattern);
    scheme:=coalesce(r[1],b[1]);
    IF scheme IS NULL OR scheme !~ '^[A-Za-z][A-Za-z0-9+.-]*:$' THEN RETURN NULL; END IF;
    IF r[1] IS NOT NULL THEN RETURN sparql.term(value,'uri'); END IF;
    authority:=r[3]; path:=r[5]; query:=r[6];
    IF r[1] IS NULL AND r[3] IS NULL THEN
        authority:=b[3];
        IF path='' THEN
            result:=scheme || coalesce(authority,'') || b[5] || coalesce(query,b[6],'') || coalesce(r[8],'');
            PERFORM sparql.within(octet_length(result),bytes);
            RETURN sparql.term(result,'uri');
        END IF;
        IF left(path,1)<>'/' THEN path:=CASE WHEN authority IS NOT NULL AND b[5]='' THEN '/'
            ELSE regexp_replace(b[5],'[^/]*$','') END || path; END IF;
    END IF;
    parts:=string_to_array(path,'/');
    WHILE parts[first] IN ('.','..') LOOP first:=first+1; END LOOP;
    -- The output's segments are parts[1:depth], each with the slash before it.
    FOR pos IN first..cardinality(parts) LOOP
        part:=parts[pos];
        IF pos>first THEN
            IF part='..' THEN depth:=greatest(0,depth-1); END IF;
            IF part IN ('.','..') THEN
                IF pos<cardinality(parts) THEN CONTINUE; END IF;
                part:='';
            END IF;
            part:='/' || part;
        END IF;
        depth:=depth+1; parts[depth]:=part;
    END LOOP;
    path:=array_to_string(parts[1:depth],'');
    result:=scheme || coalesce(authority,'') || path || coalesce(query,'') || coalesce(r[8],'');
    PERFORM sparql.within(octet_length(result),bytes);
    RETURN sparql.term(result,'uri');
END $$;

-- XPath's flags, and its syntax and escapes that PostgreSQL has; \w, \s and \d
-- are PostgreSQL's classes. REPLACE takes the longest match, or the shortest where
-- its quantifiers are reluctant; XPath takes the first its alternatives and
-- quantifiers give. What PostgreSQL does not share raises 0A000. A dot skips the
-- newline where a negated class does not, and 'x' starts no # comment.
CREATE FUNCTION sparql.regex_pattern(pattern text,flags text,extent boolean DEFAULT false) RETURNS text
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE class text:='(?!\^\])\^?(?:\\.|\[:[a-z]*:\]|[^][\\])+\]';
        tokens text[]; token text; greedy text; reluctant text;
BEGIN
    IF flags LIKE '%q%' THEN RETURN pattern; END IF;
    IF flags LIKE '%x%' THEN
        -- From a [ that begins no class the rest stays, or a space less could make it one.
        pattern:=regexp_replace(pattern,'(\[(?:' || class || '|(?!' || class ||
            ').*))|(\\)[\t\n\r ]*([^\t\n\r ]?)|[\t\n\r ]','\1\2\3','g');
    END IF;
    SELECT m[1] INTO token FROM regexp_matches(pattern,'\\(?:[1-9][0-9]+|.)','g') m
        WHERE m[1] LIKE '___%' OR strpos('nrt\|.?*+(){}-[]^$sSdDwW123456789',right(m[1],1))=0 LIMIT 1;
    IF token LIKE '___%' THEN
        RAISE EXCEPTION 'expected a back-reference \1 to \9, found `%`; write a digit that follows one as a class, such as \1[0]',
            left(token,20) USING ERRCODE='0A000';
    ELSIF token IS NOT NULL THEN
        RAISE EXCEPTION 'expected an XPath escape PostgreSQL has, found `%`; write a category as a POSIX class such as [[:alpha:]], [[:upper:]], [[:lower:]], [[:digit:]], [[:punct:]] or [[:space:]], and a word''s edge as (^|\W) or (\W|$)',
            token USING ERRCODE='0A000';
    END IF;
    tokens:=ARRAY(SELECT m[1] FROM
        regexp_matches(pattern,'\\.|\[' || class || '|\(\?.?|(?:[*+?]|\{[0-9]+(?:,[0-9]*)?\})\??|.','g') m);
    FOR pos IN 1..cardinality(tokens) LOOP
        token:=tokens[pos];
        CASE
        WHEN token='[' THEN
            RAISE EXCEPTION 'expected a class of characters, ranges, escapes and POSIX classes such as [:alpha:], found `%`; write a bracket in it as \[ or \]',
                left(array_to_string(tokens[pos:pos+19],''),20) USING ERRCODE='0A000';
        WHEN token IN (']','{','}') THEN
            RAISE EXCEPTION 'expected no unescaped `%` here, found `%`; write it as %',
                token,left(array_to_string(tokens[pos:pos+19],''),20),
                CASE token WHEN '{' THEN '\{, or a count such as {0,3}' ELSE '\' || token END USING ERRCODE='0A000';
        WHEN token LIKE '(?_' AND token<>'(?:' THEN
            RAISE EXCEPTION 'expected a group ( or (?:, found `%`; write a flag in the flags argument and a group that captures nothing as (?:',
                token USING ERRCODE='0A000';
        WHEN left(token,1) IN ('*','+','?','{') THEN
            IF pos=1 THEN
                RAISE EXCEPTION 'expected a pattern that starts with no quantifier, found `%`; write an asterisk as \*',
                    left(token,20) USING ERRCODE='0A000';
            ELSIF token ~ '[1-9][0-9]{3}|[3-9][0-9]{2}|2(?:[6-9][0-9]|5[6-9])' THEN
                RAISE EXCEPTION 'expected a repetition of at most 255, found `%`; write a longer one as several, such as a{255}a{45}',
                    left(token,20) USING ERRCODE='0A000';
            ELSIF token LIKE '{%' AND token NOT LIKE '%,%' THEN NULL;
            ELSIF token LIKE '_%?' THEN reluctant:=token;
            ELSE greedy:=token;
            END IF;
        WHEN token='|' THEN greedy:=token;
        WHEN token='.' AND flags NOT LIKE '%s%' THEN tokens[pos]:=E'[^\n]';
        ELSE
        END CASE;
    END LOOP;
    IF extent AND greedy IS NOT NULL AND reluctant IS NOT NULL THEN
        RAISE EXCEPTION 'expected quantifiers of one kind in REPLACE, found `%` and `%`; make every quantifier greedy, such as `[^'']*`, or every one reluctant and write no `|`',
            greedy,reluctant USING ERRCODE='0A000';
    END IF;
    RETURN array_to_string(tokens,'');
END $$;

-- The SPARQL functions, each under its name in lower case. A SQL body that
-- reads its argument once is inlined whatever the argument. PostgreSQL inlines
-- one that reads it twice only for a cheap argument, which a host's `out` is
-- not, and runs it slower than PL/pgSQL: those are PL/pgSQL, and so are those
-- that answer a term built from a computed value, which sparql.term reads twice.

-- The value of a string, simple or language-tagged, else NULL.
CREATE FUNCTION sparql.string(term jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal' AND (NOT term ? 'datatype' OR term ? 'xml:lang'
                     OR term->>'datatype'='http://www.w3.org/2001/XMLSchema#string')
                THEN term->>'value' END
$$;

-- The value of a simple literal, else NULL.
CREATE FUNCTION sparql.simple(term jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal' AND NOT term ? 'xml:lang'
                     AND (NOT term ? 'datatype' OR term->>'datatype'='http://www.w3.org/2001/XMLSchema#string')
                THEN term->>'value' END
$$;

-- The second string of a pair that is compatible (section 17.4.3.1.2), else NULL.
CREATE FUNCTION sparql.argument(term jsonb,other jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN NOT other ? 'xml:lang' OR lower((term->>'xml:lang') COLLATE "C")=lower((other->>'xml:lang') COLLATE "C")
                THEN sparql.string(other) END
$$;

-- The string value with the term's language tag.
CREATE FUNCTION sparql.tagged(value text,term jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN value IS NULL THEN NULL
                WHEN NOT term ? 'xml:lang' THEN jsonb_object(ARRAY['type','literal','value',value])
                ELSE jsonb_object(ARRAY['type','literal','value',value,'xml:lang',lower((term->>'xml:lang') COLLATE "C")]) END
$$;

CREATE FUNCTION sparql.str(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE WHEN term->>'type' IN ('uri','literal') THEN sparql.term(term->>'value') END;
END $$;

CREATE FUNCTION sparql.lang(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE WHEN term->>'type'='literal' THEN sparql.term(coalesce(lower((term->>'xml:lang') COLLATE "C"),'')) END;
END $$;

CREATE FUNCTION sparql.datatype(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE WHEN term->>'type'='literal' THEN sparql.term(
        CASE WHEN term ? 'xml:lang' THEN 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'
             ELSE coalesce(term->>'datatype','http://www.w3.org/2001/XMLSchema#string') END,'uri') END;
END $$;

CREATE FUNCTION sparql.isiri(term jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$ SELECT term->>'type'='uri' $$;

CREATE FUNCTION sparql.isuri(term jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$ SELECT term->>'type'='uri' $$;

CREATE FUNCTION sparql.isblank(term jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$ SELECT term->>'type'='bnode' $$;

CREATE FUNCTION sparql.isliteral(term jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$ SELECT term->>'type'='literal' $$;

CREATE FUNCTION sparql.isnumeric(term jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.numeric(term) IS NOT NULL;
END $$;

CREATE FUNCTION sparql.sameterm(a jsonb,b jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN a=b OR sparql.normalize(a)=sparql.normalize(b);
END $$;

CREATE FUNCTION sparql.strdt(term jsonb,datatype jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE WHEN datatype->>'type'='uri' AND datatype->>'value'<>'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'
                THEN sparql.term(sparql.simple(term),'literal',datatype->>'value') END;
END $$;

CREATE FUNCTION sparql.strlang(term jsonb,lang jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.simple(term); tag text := sparql.simple(lang);
BEGIN
    RETURN CASE WHEN value IS NOT NULL AND tag ~ '^[A-Za-z]+(-[A-Za-z0-9]+)*$'
                THEN jsonb_object(ARRAY['type','literal','value',value,'xml:lang',lower(tag COLLATE "C")]) END;
END $$;

-- The number in the term's numeric kind. Decimal arithmetic forgets the sign
-- of zero; XPath keeps it, observable in floating division.
CREATE FUNCTION sparql.rounded(value numeric,term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE kind text := sparql.numeric_kind(term->>'datatype');
BEGIN
    IF value=0 AND left(term->>'value',1)='-' AND kind IN ('float','double') THEN
        RETURN sparql.term('-0','literal',term->>'datatype');
    END IF;
    RETURN sparql.number(value,kind);
END $$;

CREATE FUNCTION sparql.abs(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.number(abs(sparql.numeric(term)),sparql.numeric_kind(term->>'datatype'));
END $$;

CREATE FUNCTION sparql.ceil(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.rounded(ceil(sparql.numeric(term)),term);
END $$;

CREATE FUNCTION sparql.floor(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.rounded(floor(sparql.numeric(term)),term);
END $$;

CREATE FUNCTION sparql.round(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.rounded(floor(sparql.numeric(term)+0.5),term);
END $$;

CREATE FUNCTION sparql.rand() RETURNS jsonb
LANGUAGE plpgsql VOLATILE PARALLEL RESTRICTED AS $$
DECLARE value text := sparql.format_double(random());
BEGIN
    RETURN sparql.term(value,'literal','http://www.w3.org/2001/XMLSchema#double');
END $$;

CREATE FUNCTION sparql.uuid() RETURNS jsonb
LANGUAGE sql VOLATILE PARALLEL SAFE AS $$
    SELECT jsonb_object(ARRAY['type','uri','value','urn:uuid:' || gen_random_uuid()::text])
$$;

CREATE FUNCTION sparql.struuid() RETURNS jsonb
LANGUAGE sql VOLATILE PARALLEL SAFE AS $$
    SELECT jsonb_object(ARRAY['type','literal','value',gen_random_uuid()::text])
$$;

CREATE FUNCTION sparql.now() RETURNS jsonb
LANGUAGE sql STABLE PARALLEL SAFE AS $$
    SELECT jsonb_object(ARRAY['type','literal','value',
        to_char(transaction_timestamp() AT TIME ZONE 'UTC','YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'datatype','http://www.w3.org/2001/XMLSchema#dateTime'])
$$;

-- The lexical form of a valid xsd:dateTime, else NULL.
CREATE FUNCTION sparql.date_time(term jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN term->>'type'='literal' AND NOT term ? 'xml:lang'
                     AND term->>'datatype'='http://www.w3.org/2001/XMLSchema#dateTime'
                     AND sparql.instant(term->>'value',term->>'datatype') IS NOT NULL
                THEN term->>'value' END
$$;

CREATE FUNCTION sparql.zone(lexical text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT substring(lexical FROM '(Z|[+-][0-9]{2}:[0-9]{2})$')
$$;

-- The dateTime's clock reading, which no timezone moves: the parts belong to
-- the lexical form.
CREATE FUNCTION sparql.clock(term jsonb) RETURNS timestamp
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE lexical text := sparql.date_time(term); zone text := coalesce(sparql.zone(lexical),'');
BEGIN
    RETURN sparql.instant(left(lexical,length(lexical)-length(zone)) || 'Z',term->>'datatype') AT TIME ZONE 'UTC';
END $$;

CREATE FUNCTION sparql.integer(value integer) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT sparql.term(value::text,'literal','http://www.w3.org/2001/XMLSchema#integer')
$$;

CREATE FUNCTION sparql.year(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part integer := extract(year FROM sparql.clock(term));
BEGIN
    RETURN sparql.integer(part);
END $$;

CREATE FUNCTION sparql.month(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part integer := extract(month FROM sparql.clock(term));
BEGIN
    RETURN sparql.integer(part);
END $$;

CREATE FUNCTION sparql.day(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part integer := extract(day FROM sparql.clock(term));
BEGIN
    RETURN sparql.integer(part);
END $$;

CREATE FUNCTION sparql.hours(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part integer := extract(hour FROM sparql.clock(term));
BEGIN
    RETURN sparql.integer(part);
END $$;

CREATE FUNCTION sparql.minutes(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part integer := extract(minute FROM sparql.clock(term));
BEGIN
    RETURN sparql.integer(part);
END $$;

CREATE FUNCTION sparql.seconds(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE part text := sparql.format_decimal(extract(second FROM sparql.clock(term)));
BEGIN
    RETURN sparql.term(part,'literal','http://www.w3.org/2001/XMLSchema#decimal');
END $$;

CREATE FUNCTION sparql.timezone(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE zone text := sparql.zone(sparql.date_time(term)); minutes integer; result text := 'PT0S';
BEGIN
    IF zone IS NULL THEN RETURN NULL; END IF;
    IF zone<>'Z' THEN
        minutes := substring(zone FROM 2 FOR 2)::integer*60 + substring(zone FROM 5 FOR 2)::integer;
        IF minutes>0 THEN
            result := CASE WHEN left(zone,1)='-' THEN '-' ELSE '' END || 'PT' ||
                CASE WHEN minutes/60>0 THEN (minutes/60)::text || 'H' ELSE '' END ||
                CASE WHEN minutes%60>0 THEN (minutes%60)::text || 'M' ELSE '' END;
        END IF;
    END IF;
    RETURN sparql.term(result,'literal','http://www.w3.org/2001/XMLSchema#dayTimeDuration');
END $$;

CREATE FUNCTION sparql.tz(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE lexical text := sparql.date_time(term); zone text := coalesce(sparql.zone(lexical),'');
BEGIN
    RETURN CASE WHEN lexical IS NOT NULL THEN sparql.term(zone) END;
END $$;

CREATE FUNCTION sparql.strlen(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN sparql.integer(length(sparql.string(term)));
END $$;

-- LCASE and UCASE: full case mapping makes at most three times a string's
-- bytes (U+0390 to U+0399 U+0308 U+0301), so the value is built, then raises
-- SQLSTATE 54000 once it passes the byte limit.
CREATE FUNCTION sparql.lcase(bytes bigint,term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := lower(sparql.string(term) COLLATE "pg_unicode_fast");
BEGIN
    PERFORM sparql.within(octet_length(value),bytes);
    RETURN sparql.tagged(value,term);
END $$;

CREATE FUNCTION sparql.ucase(bytes bigint,term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := upper(sparql.string(term) COLLATE "pg_unicode_fast");
BEGIN
    PERFORM sparql.within(octet_length(value),bytes);
    RETURN sparql.tagged(value,term);
END $$;

-- SQLSTATE 54000 before the join once the parts' bytes pass the byte limit.
CREATE FUNCTION sparql.concat(bytes bigint,terms jsonb[]) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE term jsonb; parts text[] := '{}'; result text; total bigint := 0;
    tag text := lower((terms[1]->>'xml:lang') COLLATE "C");
BEGIN
    FOREACH term IN ARRAY terms LOOP
        parts := array_append(parts,sparql.string(term));
        total := total+octet_length(parts[cardinality(parts)]);
        IF lower((term->>'xml:lang') COLLATE "C") IS DISTINCT FROM tag THEN tag := NULL; END IF;
    END LOOP;
    IF array_position(parts,NULL) IS NOT NULL THEN RETURN NULL; END IF;
    PERFORM sparql.within(total,bytes);
    result := array_to_string(parts,'');
    RETURN CASE WHEN tag IS NULL THEN sparql.term(result) ELSE sparql.tagged(result,terms[1]) END;
END $$;

-- The value of an xsd:integer or a type derived from it, else NULL.
CREATE FUNCTION sparql.whole(term jsonb) RETURNS numeric
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN CASE WHEN sparql.numeric_kind(term->>'datatype')='integer' THEN sparql.numeric(term) END;
END $$;

CREATE FUNCTION sparql.substr(term jsonb,start jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); first numeric := sparql.whole(start);
BEGIN
    IF value IS NULL OR first IS NULL THEN RETURN NULL; END IF;
    value := substring(value FROM greatest(1,least(length(value)+1,first))::integer);
    RETURN sparql.tagged(value,term);
END $$;

CREATE FUNCTION sparql.substr(term jsonb,start jsonb,count jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); first numeric := sparql.whole(start);
        amount numeric := sparql.whole(count); pos integer;
BEGIN
    IF value IS NULL OR first IS NULL OR amount IS NULL THEN RETURN NULL; END IF;
    pos := greatest(1,least(length(value)+1,first))::integer;
    value := substring(value FROM pos FOR greatest(0,least(length(value)+1,first+amount)-pos)::integer);
    RETURN sparql.tagged(value,term);
END $$;

CREATE FUNCTION sparql.contains(term jsonb,other jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN strpos(sparql.string(term),sparql.argument(term,other))>0;
END $$;

CREATE FUNCTION sparql.strstarts(term jsonb,other jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN starts_with(sparql.string(term),sparql.argument(term,other));
END $$;

CREATE FUNCTION sparql.strends(term jsonb,other jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE tail text := sparql.argument(term,other);
BEGIN
    RETURN right(sparql.string(term),length(tail))=tail;
END $$;

CREATE FUNCTION sparql.strbefore(term jsonb,other jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); pos integer := strpos(value,sparql.argument(term,other)); result text;
BEGIN
    IF pos=0 THEN RETURN sparql.term(''); END IF;
    result := left(value,pos-1);
    RETURN sparql.tagged(result,term);
END $$;

CREATE FUNCTION sparql.strafter(term jsonb,other jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); head text := sparql.argument(term,other);
        pos integer := strpos(value,head); result text;
BEGIN
    IF pos=0 THEN RETURN sparql.term(''); END IF;
    result := substring(value FROM pos+length(head));
    RETURN sparql.tagged(result,term);
END $$;

CREATE FUNCTION sparql.langmatches(term jsonb,range jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE tag text := lower(sparql.string(term) COLLATE "C"); match text := lower(sparql.argument(term,range) COLLATE "C");
BEGIN
    RETURN CASE WHEN match='*' THEN tag<>'' ELSE tag=match OR starts_with(tag,match || '-') END;
END $$;

-- PostgreSQL's flags for XPath's, NULL where one is no flag.
CREATE FUNCTION sparql.regex_flags(flags text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT CASE WHEN flags !~ '[^ismxq]' THEN
        CASE WHEN flags LIKE '%q%' THEN 'q' WHEN flags LIKE '%m%' THEN 'w' ELSE 's' END ||
        CASE WHEN flags LIKE '%i%' THEN 'i' ELSE '' END END
$$;

-- The text after "invalid regular expression: " is of PostgreSQL's regerrs.h,
-- which no server translates.
CREATE FUNCTION sparql.regex_limit(pattern text,message text) RETURNS void
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    IF message ~ 'too complex|out of memory|too many colors' THEN
        RAISE EXCEPTION 'expected a pattern PostgreSQL can compile, found `%`: %; write fewer or smaller repetitions, or fewer distinct characters',
            pattern,message USING ERRCODE='0A000';
    END IF;
END $$;

CREATE FUNCTION sparql.matches(term jsonb,pattern text,flags text) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    RETURN regexp_like(sparql.string(term),pattern COLLATE "pg_unicode_fast",flags);
EXCEPTION WHEN invalid_regular_expression THEN
    PERFORM sparql.regex_limit(pattern,SQLERRM);
    RETURN NULL;
END $$;

-- Inlined, so the planner translates a constant pattern once for all rows.
CREATE FUNCTION sparql.regex(term jsonb,pattern jsonb,flags jsonb DEFAULT '{"type":"literal","value":""}') RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT sparql.matches(term,sparql.regex_pattern(sparql.simple(pattern),sparql.simple(flags)),
                          sparql.regex_flags(sparql.simple(flags)))
$$;

-- PostgreSQL's replacement for XPath's, NULL where it is none.
CREATE FUNCTION sparql.replacement(replacement text,flags text) RETURNS text
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE items text[]:=ARRAY(SELECT m[1] FROM regexp_matches(replacement,'[^\\$]+|\\[\\$]|\$[0-9]*|\\','g') m);
        item text;
BEGIN
    IF flags LIKE '%q%' THEN RETURN replace(replacement,E'\\',E'\\\\'); END IF;
    FOR pos IN 1..cardinality(items) LOOP
        item:=items[pos];
        IF item IN ('$',E'\\') THEN RETURN NULL;
        ELSIF item=E'\\$' THEN items[pos]:='$';
        ELSIF item='$0' THEN items[pos]:=E'\\&';
        ELSIF item ~ '^\$..' THEN
            RAISE EXCEPTION 'expected $0 to $9, found `%`; write one digit after $, and no digit of the text after it',
                item USING ERRCODE='0A000';
        ELSIF item LIKE '$_' THEN items[pos]:=E'\\' || right(item,1);
        END IF;
    END LOOP;
    RETURN array_to_string(items,'');
END $$;

-- SQLSTATE 54000 before the replacement once its value's bytes pass the byte
-- limit: the string's without its matches, plus the replacement's literal
-- bytes once per match and, for each \& or \1 to \9 in it, that group's bytes
-- in every match. Each count is the bytes of the string with every match
-- replaced by '', by 'x' or by one group, none past twice its bytes and one.
CREATE FUNCTION sparql.replaced(bytes bigint,term jsonb,regex text,replacement text,flags text) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); result text; rest bigint; total bigint; literal bigint; ref text;
BEGIN
    IF regexp_like('',regex COLLATE "pg_unicode_fast",flags) OR value IS NULL THEN RETURN NULL; END IF;
    rest := octet_length(regexp_replace(value,regex COLLATE "pg_unicode_fast",'',flags || 'g'));
    total := rest;
    literal := octet_length(regexp_replace(replacement,'\\([&1-9])|\\(\\)','\2','g'));
    IF literal>0 THEN
        total := total+literal*(octet_length(regexp_replace(value,regex COLLATE "pg_unicode_fast",'x',flags || 'g'))-rest);
    END IF;
    FOR ref IN SELECT m[1] FROM regexp_matches(replacement,'\\([&1-9\\])','g') m WHERE m[1]<>E'\\' LOOP
        total := total+octet_length(regexp_replace(value,regex COLLATE "pg_unicode_fast",E'\\' || ref,flags || 'g'))-rest;
    END LOOP;
    PERFORM sparql.within(total,bytes);
    result := regexp_replace(value,regex COLLATE "pg_unicode_fast",replacement,flags || 'g');
    RETURN sparql.tagged(result,term);
EXCEPTION WHEN invalid_regular_expression THEN
    PERFORM sparql.regex_limit(regex,SQLERRM);
    RETURN NULL;
END $$;

-- Inlined, as sparql.regex is.
CREATE FUNCTION sparql.replace(bytes bigint,term jsonb,pattern jsonb,replacement jsonb,
    flags jsonb DEFAULT '{"type":"literal","value":""}') RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT sparql.replaced(bytes,term,sparql.regex_pattern(sparql.simple(pattern),sparql.simple(flags),true),
                           sparql.replacement(sparql.simple(replacement),sparql.simple(flags)),
                           sparql.regex_flags(sparql.simple(flags)))
$$;

-- Each UTF-8 byte of the string but an unreserved character's becomes three,
-- so the value's bytes are known, and checked against the byte limit, before
-- it is built.
CREATE FUNCTION sparql.encode_for_uri(bytes bigint,term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE value text := sparql.string(term); octets bytea := convert_to(value,'UTF8'); result text;
BEGIN
    IF octets IS NULL THEN RETURN NULL; END IF;
    PERFORM sparql.within(length(octets)
        +2*octet_length(convert_to(regexp_replace(value COLLATE "C",'[-.0-9A-Z_a-z~]','','g'),'UTF8')),bytes);
    SELECT string_agg(CASE WHEN byte BETWEEN 65 AND 90 OR byte BETWEEN 97 AND 122 OR byte BETWEEN 48 AND 57
                                OR byte IN (45,46,95,126) THEN chr(byte)
                           ELSE '%' || upper(lpad(to_hex(byte),2,'0')) END,'' ORDER BY pos) INTO result
      FROM (SELECT pos,get_byte(octets,pos) AS byte FROM generate_series(0,length(octets)-1) pos) t;
    result := coalesce(result,'');
    RETURN sparql.term(result);
END $$;

CREATE FUNCTION sparql.md5(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE hash text := md5(convert_to(sparql.simple(term),'UTF8'));
BEGIN
    RETURN sparql.term(hash);
END $$;

CREATE FUNCTION sparql.sha256(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE hash text := encode(sha256(convert_to(sparql.simple(term),'UTF8')),'hex');
BEGIN
    RETURN sparql.term(hash);
END $$;

CREATE FUNCTION sparql.sha384(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE hash text := encode(sha384(convert_to(sparql.simple(term),'UTF8')),'hex');
BEGIN
    RETURN sparql.term(hash);
END $$;

CREATE FUNCTION sparql.sha512(term jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
DECLARE hash text := encode(sha512(convert_to(sparql.simple(term),'UTF8')),'hex');
BEGIN
    RETURN sparql.term(hash);
END $$;

CREATE FUNCTION sparql.in(value jsonb,items jsonb[]) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE item jsonb; equal boolean; failed boolean := false;
BEGIN
    FOREACH item IN ARRAY coalesce(items,'{}'::jsonb[]) LOOP
        equal := sparql.equal(value,item);
        IF equal THEN RETURN true; END IF;
        failed := failed OR equal IS NULL;
    END LOOP;
    RETURN CASE WHEN failed THEN NULL ELSE false END;
END $$;

-- An aggregate holds one term, one running total or one byte count per group,
-- never the group's terms. SUM/AVG must not silently discard a nonnumeric
-- value. MIN/MAX return original terms; SUM/AVG are promoted and spelled as
-- arithmetic promotes and spells them.

-- The term MIN or MAX keeps, and its key in ORDER BY's order.
CREATE TYPE sparql.extreme AS (term jsonb, key sparql.order_key);

-- MIN keeps the first term in ORDER BY's order, which an unbound one leads
-- (SPARQL 1.1 sections 18.5.1.5, 15.1); of equal terms, MIN and MAX keep the
-- first.
CREATE FUNCTION sparql.min_step(best sparql.extreme, term jsonb) RETURNS sparql.extreme
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE key sparql.order_key := sparql.order(term);
BEGIN
    IF best IS NULL OR key<best.key THEN RETURN ROW(term,key)::sparql.extreme; END IF;
    RETURN best;
END $$;

CREATE FUNCTION sparql.max_step(best sparql.extreme, term jsonb) RETURNS sparql.extreme
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE key sparql.order_key := sparql.order(term);
BEGIN
    IF best IS NULL OR key>best.key THEN RETURN ROW(term,key)::sparql.extreme; END IF;
    RETURN best;
END $$;

CREATE FUNCTION sparql.extreme_term(best sparql.extreme) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT (best).term
$$;

CREATE AGGREGATE sparql.min(jsonb) (SFUNC=sparql.min_step, STYPE=sparql.extreme,
    FINALFUNC=sparql.extreme_term, PARALLEL=SAFE);
CREATE AGGREGATE sparql.max(jsonb) (SFUNC=sparql.max_step, STYPE=sparql.extreme,
    FINALFUNC=sparql.extreme_term, PARALLEL=SAFE);

-- SUM's and AVG's walk over the terms: kind is the running total's datatype,
-- total its exact value and sum its floating one; long says total's text may
-- pass 16,383 bytes, amount counts the terms and failed makes the result
-- unbound. While kind is integer or decimal, terms add up as numeric and one
-- that is no number makes the total NaN. The first float or double turns it
-- into one; from there each term is added as arithmetic adds it to the running
-- total's term, so that term must read back: an exact one past 16,383 bytes is
-- no number, and a term that is no number or an overflow makes the result
-- unbound. A float total keeps its real: the real's shortest text, which
-- arithmetic spells, reads back as the same real.
CREATE TYPE sparql.total AS (kind text, total numeric, long boolean, sum double precision,
                             amount bigint, failed boolean);

CREATE FUNCTION sparql.total_step(walk sparql.total, term jsonb) RETURNS sparql.total
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE datatype text := term->>'datatype'; next text; value double precision;
BEGIN
    walk.amount := walk.amount+1;
    IF walk.failed THEN RETURN walk; END IF;
    IF walk.kind IN ('integer','decimal') AND (datatype IN ('http://www.w3.org/2001/XMLSchema#float',
                                                            'http://www.w3.org/2001/XMLSchema#double')) IS NOT TRUE THEN
        -- Shorter than 16,002 bytes otherwise.
        IF abs(walk.total)>=1e8000 OR scale(walk.total)>8000 THEN
            walk.long := walk.long OR (octet_length(sparql.format_decimal(walk.total))>16383) IS TRUE;
        END IF;
        walk.total := walk.total + coalesce(sparql.numeric(term),'NaN');
        IF datatype='http://www.w3.org/2001/XMLSchema#decimal' THEN walk.kind := 'decimal'; END IF;
        RETURN walk;
    END IF;
    next := CASE WHEN walk.kind='double' OR datatype LIKE '%#double' THEN 'double' ELSE 'float' END;
    IF walk.kind IN ('integer','decimal') THEN
        IF walk.long THEN walk.failed := true; RETURN walk; END IF;
        walk.sum := sparql.floating(sparql.number(walk.total,walk.kind),next);
    END IF;
    value := sparql.floating(term,next);
    IF walk.sum IS NULL OR value IS NULL THEN walk.failed := true; RETURN walk; END IF;
    IF abs(walk.sum)<1e37 AND abs(value)<1e37 THEN
        walk.sum := CASE next WHEN 'float' THEN walk.sum::real+value::real ELSE walk.sum+value END;
    ELSE
        BEGIN
            walk.sum := CASE next WHEN 'float' THEN walk.sum::real+value::real ELSE walk.sum+value END;
        EXCEPTION WHEN numeric_value_out_of_range THEN
            walk.failed := true;
        END;
    END IF;
    walk.kind := next;
    RETURN walk;
END $$;

CREATE FUNCTION sparql.total_sum(walk sparql.total) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE SET extra_float_digits=3 AS $$
BEGIN
    IF walk.failed THEN RETURN NULL; END IF;
    IF walk.kind IN ('float','double') THEN
        RETURN sparql.term(sparql.format_double(CASE walk.kind WHEN 'float' THEN walk.sum::real::text::double precision
                                                               ELSE walk.sum END),
                           'literal','http://www.w3.org/2001/XMLSchema#' || walk.kind);
    END IF;
    IF walk.total='NaN' THEN RETURN NULL; END IF;
    RETURN sparql.number(walk.total,walk.kind);
END $$;

CREATE FUNCTION sparql.total_avg(walk sparql.total) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE SET extra_float_digits=3 AS $$
DECLARE result jsonb := sparql.total_sum(walk);
BEGIN
    IF result IS NULL OR walk.amount=0 THEN RETURN result; END IF;
    RETURN sparql.arithmetic('/',result,sparql.number(walk.amount,'integer'));
END $$;

CREATE AGGREGATE sparql.sum(jsonb) (SFUNC=sparql.total_step, STYPE=sparql.total,
    FINALFUNC=sparql.total_sum, INITCOND='(integer,0,f,,0,f)', PARALLEL=SAFE);
CREATE AGGREGATE sparql.avg(jsonb) (SFUNC=sparql.total_step, STYPE=sparql.total,
    FINALFUNC=sparql.total_avg, INITCOND='(integer,0,f,,0,f)', PARALLEL=SAFE);

-- GROUP_CONCAT's guard: over the rows string_agg reads, it counts the bytes
-- string_agg's text holds and raises 54000 once they pass the byte limit;
-- false once an unbound term or a blank node makes the text unbound. Its
-- state: bytes, NULL while no value is held, each value after the first
-- adding the separator's; bound, the result.
CREATE TYPE sparql.concat_count AS (bytes bigint, bound boolean);

CREATE FUNCTION sparql.count_step(count sparql.concat_count, term jsonb, separator text, bytes bigint)
RETURNS sparql.concat_count LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE AS $$
DECLARE piece text := term->>'value';
BEGIN
    IF piece IS NOT NULL THEN
        count.bytes := coalesce(count.bytes+octet_length(separator),0)+octet_length(piece);
        -- Called only to raise: a call on every row costs a fifth more.
        IF count.bytes>bytes THEN PERFORM sparql.within(count.bytes,bytes); END IF;
    END IF;
    count.bound := count.bound AND term IS NOT NULL AND term->>'type' IS DISTINCT FROM 'bnode';
    RETURN count;
END $$;

CREATE FUNCTION sparql.count_bound(count sparql.concat_count) RETURNS boolean
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
    SELECT (count).bound
$$;

CREATE AGGREGATE sparql.concat_bound(jsonb, text, bigint) (SFUNC=sparql.count_step, STYPE=sparql.concat_count,
    FINALFUNC=sparql.count_bound, INITCOND='(,t)', PARALLEL=SAFE);
