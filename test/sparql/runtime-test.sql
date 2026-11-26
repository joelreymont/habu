-- Expression results and errors through PostgreSQL, in the compiler's RDF terms.
BEGIN;
DO $$
DECLARE
    x text := 'http://www.w3.org/2001/XMLSchema#';
    one jsonb := sparql.term('1','literal',x || 'integer');
    decimal jsonb := sparql.term('1.0','literal',x || 'decimal');
    nan jsonb := sparql.term('NaN','literal',x || 'double');
    float_digits text := current_setting('extra_float_digits');
BEGIN
    IF sparql.equal(one,decimal) IS DISTINCT FROM true THEN RAISE EXCEPTION 'numeric equality'; END IF;
    IF sparql.normalize(one)=sparql.normalize(decimal) THEN RAISE EXCEPTION 'term identity'; END IF;
    IF sparql.equal(sparql.term('16777217','literal',x || 'integer'),
            sparql.term('16777216','literal',x || 'float')) IS DISTINCT FROM true
       OR sparql.equal(sparql.term('0.1','literal',x || 'float'),
            sparql.term('0.10000000149011612','literal',x || 'double')) IS DISTINCT FROM true
       THEN RAISE EXCEPTION 'numeric operand promotion'; END IF;
    IF sparql.arithmetic('+',sparql.term('16777217','literal',x || 'integer'),
            sparql.term('-16777216','literal',x || 'float')) IS DISTINCT FROM sparql.number(0,'float')
       THEN RAISE EXCEPTION 'floating arithmetic promotes operands'; END IF;
    IF sparql.arithmetic('+',sparql.term('0.1','literal',x || 'double'),
            sparql.term('0.2','literal',x || 'double')) IS DISTINCT FROM sparql.number(0.30000000000000004,'double')
       OR sparql.arithmetic('+',sparql.term('0.1','literal',x || 'decimal'),
            sparql.term('0.2','literal',x || 'decimal')) IS DISTINCT FROM sparql.number(0.3,'decimal')
       THEN RAISE EXCEPTION 'floating and decimal arithmetic'; END IF;
    IF sparql.isnumeric(sparql.term('1','literal',
            'integerhttp://www.w3.org/2001/XMLSchema#')) IS DISTINCT FROM false
       THEN RAISE EXCEPTION 'numeric datatype identity'; END IF;
    IF EXISTS (SELECT FROM (SELECT sparql.term('-1','literal',x || 'negativeInteger')) v(term)
        CROSS JOIN LATERAL (VALUES (sparql.abs(term)),(sparql.ceil(term)),(sparql.floor(term)),(sparql.round(term))) f(result)
        WHERE result->>'datatype' IS DISTINCT FROM x || 'integer') THEN RAISE EXCEPTION 'numeric function result type'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind) CROSS JOIN (VALUES
        ('+','INF','-INF','NaN'),('+','INF','1','INF'),('-','-INF','1','-INF'),
        ('*','0','INF','NaN'),('/','1','0','INF'),('/','-1','0','-INF'),
        ('/','0','0','NaN'),('/','INF','INF','NaN'),('+','NaN','1','NaN'),('/','1','INF','0')) v(op,a,b,expected)
        WHERE sparql.arithmetic(op,sparql.term(a,'literal',x || kind),
              sparql.term(b,'literal',x || kind)) IS DISTINCT FROM sparql.term(expected,'literal',x || kind))
       THEN RAISE EXCEPTION 'floating nonfinite arithmetic'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind) CROSS JOIN (VALUES
        ('1','-INF'),('-1','INF'),('0','NaN')) v(a,expected)
        WHERE sparql.arithmetic('/',sparql.term(a,'literal',x || kind),
              sparql.term('-0','literal',x || kind)) IS DISTINCT FROM sparql.term(expected,'literal',x || kind))
       THEN RAISE EXCEPTION 'division by negative zero'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind)
        WHERE sparql.arithmetic('/',sparql.term('1','literal',x || kind),
              sparql.arithmetic('/',sparql.term('0','literal',x || kind),
                  sparql.term('-1','literal',x || kind)))
              IS DISTINCT FROM sparql.term('-INF','literal',x || kind))
       THEN RAISE EXCEPTION 'computed negative zero survives'; END IF;
    PERFORM set_config('extra_float_digits','-3',true);
    IF sparql.numeric(sparql.term('0.1','literal',x || 'float')) IS DISTINCT FROM 0.10000000149011612
       OR sparql.arithmetic('+',sparql.term('0.1','literal',x || 'double'),
              sparql.term('0.2','literal',x || 'double'))
              IS DISTINCT FROM sparql.term('0.30000000000000004','literal',x || 'double')
       OR sparql.cast(x || 'double',sparql.term('0.1','literal',x || 'float'))
              IS DISTINCT FROM sparql.term('0.10000000149011612','literal',x || 'double')
       THEN RAISE EXCEPTION 'floating precision is session independent'; END IF;
    PERFORM set_config('extra_float_digits',float_digits,true);
    IF sparql.relation('<',nan,one) IS DISTINCT FROM false THEN RAISE EXCEPTION 'NaN comparison'; END IF;
    IF sparql.equal(nan,nan) IS DISTINCT FROM false THEN RAISE EXCEPTION 'NaN equality'; END IF;
    IF sparql.ebv(sparql.term('','literal')) IS DISTINCT FROM false THEN RAISE EXCEPTION 'empty string EBV'; END IF;
    IF sparql.ebv(sparql.term('bad','literal',x || 'integer')) IS DISTINCT FROM false THEN RAISE EXCEPTION 'invalid numeric EBV'; END IF;
    IF sparql.arithmetic('/',one,sparql.term('0','literal',x || 'integer')) IS NOT NULL THEN RAISE EXCEPTION 'zero division'; END IF;
    IF sparql.arithmetic('/',one,one)->>'datatype' IS DISTINCT FROM x || 'decimal' THEN RAISE EXCEPTION 'integer division promotion'; END IF;
    IF EXISTS (SELECT FROM (VALUES (NULL::jsonb,sparql.term('b0','bnode')),
            (sparql.term('b0','bnode'),sparql.term('urn:x','uri')),(sparql.term('urn:x','uri'),one),
            (one,sparql.term('10','literal',x || 'integer')),(sparql.term('B'),sparql.term('a')),
            (sparql.term('a'),sparql.term('b')),(sparql.term('b'),sparql.term('é'))) v(low,high)
        WHERE sparql.order(low)>=sparql.order(high))
       THEN RAISE EXCEPTION 'ordering: unbound, blank, IRI, literal; numbers by value; code points'; END IF;
    IF sparql.strlang(sparql.term('hello'),sparql.term('EN'))->>'xml:lang' IS DISTINCT FROM 'en' THEN RAISE EXCEPTION 'language normalization'; END IF;
    IF sparql.abs(sparql.term('-2.5','literal',x || 'decimal'))->>'value' IS DISTINCT FROM '2.5' THEN RAISE EXCEPTION 'ABS'; END IF;
    IF sparql.substr(sparql.term('AéB'),sparql.term('2','literal',x || 'integer'),one)->>'value' IS DISTINCT FROM 'é' THEN RAISE EXCEPTION 'Unicode substring'; END IF;
    IF sparql.regex(sparql.term(E'A\nB'),sparql.term('A.B')) IS DISTINCT FROM false THEN RAISE EXCEPTION 'regex default newline'; END IF;
    IF sparql.regex(sparql.term(E'A\nB'),sparql.term('A.B'),sparql.term('s')) IS DISTINCT FROM true THEN RAISE EXCEPTION 'regex dotall'; END IF;
    IF EXISTS (SELECT FROM (VALUES
        (E'A\nB','A[^C]B','',true),(E'A\nB','^B','m',true),
        ('A.B',E'A\\.B','',true),('A.B','A[.]B','',true),
        ('a?+*.{}()[]c','a?+*.{}()[]C','iq',true),
        ('a # b','a # b','qx',true),('a#b','a # b','x',true),
        ('a b',E'a\\ sb','x',true),('ab','a[ ]b','x',false),
        (E'\n\r\t\\|.?*+(){}-[]^$',E'\\n\\r\\t\\\\\\|\\.\\?\\*\\+\\(\\)\\{\\}\\-\\[\\]\\^\\$','',true),
        ('aa',E'^(a)\\1$','',true),('a1-',E'^[\\w\\-]+[^\\s[:alpha:]]$','',true),('a',E'a\\','',NULL),
        ('b','a+?|b','',true),('ya','(x|y)a+?','',true),
        ('\p','^[\\p]+$','',true),('(i','^\(?i$','',true),('a bc','^a[ ]b c$','x',true),
        ('[\]a','^\[\ \\] \ w$','x',true),('aa','^a {2} ?$','x',true)
      ) t(value,pattern,flags,expected)
      WHERE sparql.regex(sparql.term(value),
          sparql.term(pattern),sparql.term(flags)) IS DISTINCT FROM expected)
      THEN RAISE EXCEPTION 'regex flags and character classes'; END IF;
    IF sparql.replace(sparql.term('a.b'),sparql.term('.'),
            sparql.term(E'$1\\'),sparql.term('q'))->>'value' IS DISTINCT FROM E'a$1\\b'
       THEN RAISE EXCEPTION 'quoted replacement'; END IF;
    -- The test cluster's locale is C, whose classes and case are ASCII.
    IF sparql.regex(sparql.term('É'),sparql.term(E'^\\w$')) IS DISTINCT FROM true
       OR sparql.regex(sparql.term('É'),sparql.term('é'),sparql.term('i')) IS DISTINCT FROM true
       OR sparql.replace(sparql.term('É'),sparql.term('é'),sparql.term('e'),sparql.term('i'))->>'value'
          IS DISTINCT FROM 'e'
       THEN RAISE EXCEPTION 'regex classes and case are Unicode'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('[$0]','L[a]the'),('<$2$1$3>','L<ta>he'),(E'\\$','L$the'),(E'\\$1','L$1the'),
            (E'\\\\',E'L\\the'),(E'\\\\$1',E'L\\athe'),('&','L&the'),('$x',NULL),('$',NULL),(E'\\1',NULL),(E'\\',NULL)
          ) t(replacement,expected)
          WHERE sparql.replace(sparql.term('Lathe'),
              sparql.term(CASE WHEN replacement LIKE '%$2%' THEN '(a)(t)' ELSE '(a)' END),
              sparql.term(replacement))->>'value' IS DISTINCT FROM expected)
       THEN RAISE EXCEPTION 'replacement'; END IF;
    IF EXISTS (SELECT FROM unnest(ARRAY['''([^'']*)''','''([a-z]{2}.*?)''']) pattern
          WHERE sparql.replace(sparql.term('say ''hi'' and ''bye'''),sparql.term(pattern),
              sparql.term('<$1>'))->>'value' IS DISTINCT FROM 'say <hi> and <bye>')
       THEN RAISE EXCEPTION 'replacement by quantifiers of one kind'; END IF;
    IF sparql.encode_for_uri(sparql.term('é /'))->>'value' IS DISTINCT FROM '%C3%A9%20%2F' THEN RAISE EXCEPTION 'URI UTF8'; END IF;
    IF sparql.timezone(sparql.term('2026-11-13T10:20:30+02:30','literal',x || 'dateTime'))->>'value' IS DISTINCT FROM 'PT2H30M' THEN RAISE EXCEPTION 'timezone'; END IF;
    IF sparql.sha256(sparql.term('abc'))->>'value' IS DISTINCT FROM 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad' THEN RAISE EXCEPTION 'SHA256'; END IF;
    IF sparql.year(sparql.term('2024-01-01T00:00:00Z')) IS NOT NULL
       OR sparql.year(sparql.term('2024-01-01','literal',x || 'date')) IS NOT NULL
       OR sparql.substr(sparql.term('abcd'),one,sparql.term('invalid')) IS NOT NULL
       OR sparql.substr(sparql.term('abcd'),decimal) IS NOT NULL
       OR sparql.regex(sparql.term('a'),sparql.term('a'),one) IS NOT NULL
       OR sparql.replace(sparql.term('a'),sparql.term(''),sparql.term('-')) IS NOT NULL
       OR sparql.sha256('{"type":"literal","value":"a","xml:lang":"en"}'::jsonb) IS NOT NULL
       THEN RAISE EXCEPTION 'function argument and pattern errors'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind)
         CROSS JOIN (VALUES ('u+','-0','-INF'),('u-','0','-INF'),('u-','-0','INF')) v(op,lexical,expected)
         WHERE sparql.arithmetic('/',one,sparql.arithmetic(op,NULL,
               sparql.term(lexical,'literal',x || kind)))->>'value' IS DISTINCT FROM expected)
       THEN RAISE EXCEPTION 'unary floating zero retains IEEE sign'; END IF;
    IF sparql.boolean(NULL) IS NOT NULL OR sparql.boolean(false)->>'value' IS DISTINCT FROM 'false' THEN RAISE EXCEPTION 'boolean error/value distinction'; END IF;
    IF sparql.in(one,ARRAY[sparql.term('error'),decimal]) IS DISTINCT FROM true
       OR sparql.in(one,ARRAY[sparql.term('error')]) IS NOT NULL THEN RAISE EXCEPTION 'IN errors'; END IF;
    IF sparql.aggregate('MIN',ARRAY[decimal,sparql.term('2','literal',x || 'integer')],false,' ') IS DISTINCT FROM decimal
       OR sparql.aggregate('SAMPLE',ARRAY[decimal],false,' ') IS DISTINCT FROM decimal THEN RAISE EXCEPTION 'selected aggregate term'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('+001','integer'),('1.00','decimal'),('1.0e+2','double'),('-0','float')) v(lexical,kind)
        WHERE sparql.normalize(sparql.term(lexical,'literal',x || kind))->>'value' IS DISTINCT FROM lexical)
       OR EXISTS (SELECT FROM (VALUES ('MIN'),('MAX'),('SAMPLE')) f(name)
          CROSS JOIN (VALUES ('+001','integer'),('1.00','decimal'),('1.0e+2','double'),('-0','float')) v(lexical,kind)
        WHERE sparql.aggregate(name,ARRAY[sparql.term(lexical,'literal',x || kind)],false,' ')
              IS DISTINCT FROM sparql.term(lexical,'literal',x || kind))
       THEN RAISE EXCEPTION 'input and selected aggregate lexical forms'; END IF;
    IF sparql.aggregate('MIN',ARRAY[sparql.term('0.1','literal',x || 'float'),
            sparql.term('0.1000000001','literal',x || 'double')],false,' ')
            IS DISTINCT FROM sparql.term('0.1000000001','literal',x || 'double')
       OR sparql.aggregate('MAX',ARRAY[sparql.term('0.1','literal',x || 'float'),
            sparql.term('0.1000000001','literal',x || 'double')],false,' ')
            IS DISTINCT FROM sparql.term('0.1','literal',x || 'float')
       THEN RAISE EXCEPTION 'mixed floating extrema retain original terms'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('SUM'),('AVG')) f(name)
        WHERE sparql.aggregate(name,ARRAY[sparql.term('16777217','literal',x || 'integer'),
            sparql.term('-16777216','literal',x || 'float')],false,' ') IS DISTINCT FROM sparql.term('0','literal',x || 'float'))
       THEN RAISE EXCEPTION 'aggregate operand promotion'; END IF;
    IF sparql.aggregate('AVG',ARRAY[one,sparql.number(2,'integer'),
            sparql.number(3,'integer'),sparql.number(4,'integer')],false,' ')
            IS DISTINCT FROM sparql.term('2.5','literal',x || 'decimal')
       THEN RAISE EXCEPTION 'computed decimal scale'; END IF;
    IF sparql.aggregate('SUM',ARRAY[one,sparql.term('error')],false,' ') IS NOT NULL THEN RAISE EXCEPTION 'SUM type error'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('SUM'),('AVG')) f(name)
        WHERE sparql.aggregate(name,ARRAY[]::jsonb[],false,' ') IS DISTINCT FROM sparql.term('0','literal',x || 'integer'))
       THEN RAISE EXCEPTION 'empty numeric aggregate'; END IF;
    -- W3C SPARQL 1.1 aggregates/agg-numeric2.ttl and the queries of
    -- agg-sum-02 and agg-avg-02 (whose HAVING retains averages <= 2.0).
    IF EXISTS (WITH input(subject,lexical,kind) AS (VALUES
            ('ints','1','integer'),('ints','2','integer'),('ints','3','integer'),
            ('decimals','1.0','decimal'),('decimals','2.2','decimal'),('decimals','3.5','decimal'),
            ('doubles','1.0E2','double'),('doubles','2.0E3','double'),('doubles','3.0E4','double'),
            ('mixed1','1','integer'),('mixed1','2.2','decimal'),
            ('mixed2','2E-1','double'),('mixed2','0.2','decimal')),
        actual AS (SELECT subject,
            sparql.aggregate('SUM',array_agg(sparql.term(lexical,'literal',x || kind)),false,' ') AS total,
            sparql.aggregate('AVG',array_agg(sparql.term(lexical,'literal',x || kind)),false,' ') AS average
            FROM input GROUP BY subject),
        expected(subject,total,total_kind,average,average_kind) AS (VALUES
            ('ints','6','integer','2','decimal'),('decimals','6.7','decimal',NULL,NULL),
            ('doubles','32100','double',NULL,NULL),('mixed1','3.2','decimal','1.6','decimal'),
            ('mixed2','0.4','double','0.2','double'))
        SELECT FROM actual a JOIN expected e USING(subject)
        WHERE a.total IS DISTINCT FROM sparql.term(e.total,'literal',x || e.total_kind)
           OR CASE WHEN sparql.relation('<=',a.average,sparql.term('2.0','literal',x || 'decimal'))
                   THEN a.average END IS DISTINCT FROM sparql.term(e.average,'literal',x || e.average_kind))
       THEN RAISE EXCEPTION 'W3C grouped SUM and AVG result terms'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('SUM'),('AVG')) f(name)
        CROSS JOIN (VALUES ('float'),('double')) d(kind) CROSS JOIN (VALUES ('NaN'),('INF'),('-INF')) v(lexical)
        WHERE sparql.aggregate(name,ARRAY[sparql.term(lexical,'literal',x || kind)],false,' ')
              IS DISTINCT FROM sparql.term(lexical,'literal',x || kind))
       THEN RAISE EXCEPTION 'aggregate nonfinite result terms'; END IF;
    IF sparql.aggregate('SUM',ARRAY[sparql.term('0.1','literal',x || 'double'),
            sparql.term('0.2','literal',x || 'double')],false,' ')
            IS DISTINCT FROM sparql.term('0.30000000000000004','literal',x || 'double')
       OR EXISTS (SELECT FROM (VALUES ('-1e-300','-1.0E-300'),('1e+300','1.0E300'),('0','0')) v(lexical,expected)
        WHERE sparql.aggregate('SUM',ARRAY[sparql.term(lexical,'literal',x || 'double')],false,' ')
              IS DISTINCT FROM sparql.term(expected,'literal',x || 'double'))
       THEN RAISE EXCEPTION 'aggregate scientific spelling retains precision and exponent'; END IF;
    IF sparql.equal(sparql.term('bad','literal',x || 'integer'),sparql.term('bad','literal',x || 'integer')) IS DISTINCT FROM true THEN RAISE EXCEPTION 'ill-typed numeric term identity'; END IF;
    IF sparql.cast(x || 'integer',sparql.term('211942')) IS DISTINCT FROM sparql.number(211942,'integer')
       OR sparql.cast(x || 'integer',sparql.term('-1.9','literal',x || 'decimal')) IS DISTINCT FROM sparql.number(-1,'integer') THEN RAISE EXCEPTION 'integer constructors'; END IF;
    IF sparql.cast(x || 'boolean',nan) IS DISTINCT FROM sparql.boolean(false)
       OR sparql.cast(x || 'double',sparql.boolean(true)) IS DISTINCT FROM sparql.number(1,'double') THEN RAISE EXCEPTION 'numeric boolean constructors'; END IF;
    IF sparql.cast(x || 'double',sparql.term('-0','literal',x || 'float'))
              IS DISTINCT FROM sparql.term('-0','literal',x || 'double')
       OR sparql.cast(x || 'float',sparql.term('-0'))
              IS DISTINCT FROM sparql.term('-0','literal',x || 'float')
       THEN RAISE EXCEPTION 'floating casts preserve negative zero'; END IF;
    IF sparql.cast(x || 'integer',sparql.term('1.9')) IS NOT NULL
       OR sparql.cast(x || 'integer',nan) IS NOT NULL
       OR sparql.cast(x || 'boolean',sparql.term('bad')) IS NOT NULL THEN RAISE EXCEPTION 'invalid constructors'; END IF;
    -- XPath "Casting to xs:string" for each numeric source.
    IF EXISTS (SELECT FROM (VALUES ('integer','+0042','42'),('integer','-7','-7'),
        ('decimal','2.00','2'),('decimal','-0.0','0'),('decimal','1.50','1.5'),('decimal','-0.25','-0.25'),
        ('double','0','0'),('double','-0.0E0','-0'),('float','-0','-0'),
        ('double','INF','INF'),('double','-INF','-INF'),('double','NaN','NaN'),('float','-INF','-INF'),
        ('double','1.5E0','1.5'),('double','1e-6','0.000001'),('double','1E2','100'),('double','-999999.5','-999999.5'),
        ('float','0.1','0.1'),('float','1e-6','0.000001'),('float','100','100'),
        ('double','1e300','1.0E300'),('double','1e6','1.0E6'),('double','9.99e-7','9.99E-7'),
        ('float','1e6','1.0E6'),('float','0.1e-6','1.0E-7')) v(kind,lexical,expected)
        WHERE sparql.cast(x || 'string',sparql.term(lexical,'literal',x || kind))
              IS DISTINCT FROM sparql.term(expected))
       THEN RAISE EXCEPTION 'numbers cast to string follow XPath'; END IF;
    IF sparql.cast(x || 'string',sparql.term('urn:zen:1','uri')) IS DISTINCT FROM sparql.term('urn:zen:1')
       OR sparql.cast(x || 'dateTime',sparql.term('2026-11-13T10:20:30Z')) IS DISTINCT FROM sparql.term('2026-11-13T10:20:30Z','literal',x || 'dateTime') THEN RAISE EXCEPTION 'string datetime constructors'; END IF;
    IF sparql.iri(sparql.term('bad space'),'http://a/b/c/d;p?q') IS NOT NULL
       OR sparql.iri(sparql.term('bad%GG'),'http://a/b/c/d;p?q') IS NOT NULL
       OR sparql.iri(sparql.term('relative'),NULL) IS NOT NULL THEN RAISE EXCEPTION 'invalid IRI'; END IF;
    -- RFC 3986 section 5.4 normal and abnormal resolution examples.
    IF EXISTS (SELECT FROM (VALUES
        ('eXAMPLE://a/./b/../b/%63/%7bfoo%7d#xyz','eXAMPLE://a/./b/../b/%63/%7bfoo%7d#xyz'),
        ('g:h','g:h'),('g','http://a/b/c/g'),('./g','http://a/b/c/g'),('g/','http://a/b/c/g/'),
        ('/g','http://a/g'),('//g','http://g'),('?y','http://a/b/c/d;p?y'),('g?y','http://a/b/c/g?y'),
        ('#s','http://a/b/c/d;p?q#s'),('g#s','http://a/b/c/g#s'),('g?y#s','http://a/b/c/g?y#s'),
        (';x','http://a/b/c/;x'),('g;x','http://a/b/c/g;x'),('g;x?y#s','http://a/b/c/g;x?y#s'),
        ('','http://a/b/c/d;p?q'),('.','http://a/b/c/'),('./','http://a/b/c/'),('..','http://a/b/'),
        ('../','http://a/b/'),('../g','http://a/b/g'),('../..','http://a/'),('../../','http://a/'),('../../g','http://a/g'),
        ('../../../g','http://a/g'),('../../../../g','http://a/g'),('/./g','http://a/g'),('/../g','http://a/g'),
        ('g.','http://a/b/c/g.'),('.g','http://a/b/c/.g'),('g..','http://a/b/c/g..'),('..g','http://a/b/c/..g'),
        ('./../g','http://a/b/g'),('./g/.','http://a/b/c/g/'),('g/./h','http://a/b/c/g/h'),('g/../h','http://a/b/c/h'),
        ('g;x=1/./y','http://a/b/c/g;x=1/y'),('g;x=1/../y','http://a/b/c/y'),
        ('g?y/./x','http://a/b/c/g?y/./x'),('g?y/../x','http://a/b/c/g?y/../x'),
        ('g#s/./x','http://a/b/c/g#s/./x'),('g#s/../x','http://a/b/c/g#s/../x'),('http:g','http:g')
      ) t(reference,expected)
       WHERE sparql.iri(sparql.term(reference),'http://a/b/c/d;p?q')->>'value' IS DISTINCT FROM expected)
       THEN RAISE EXCEPTION 'IRI reference resolution'; END IF;
END $$;
DO $$
DECLARE
    x constant text := 'http://www.w3.org/2001/XMLSchema#';
    lang jsonb := '{"type":"literal","value":"xyz","xml:lang":"en"}';
    timezone text := current_setting('TimeZone'); zone text;
BEGIN
    IF EXISTS (SELECT FROM (VALUES (x || 'integer'),('urn:unknown')) d(datatype)
        WHERE sparql.equal(lang,sparql.term('xyz','literal',datatype)) IS DISTINCT FROM false
           OR sparql.equal(sparql.term('xyz','literal',datatype),lang) IS DISTINCT FROM false)
       THEN RAISE EXCEPTION 'language-tagged literals differ from incompatible typed values'; END IF;
    IF EXISTS (SELECT FROM (VALUES (x || 'integer'),('urn:unknown')) d(datatype)
        WHERE sparql.equal(sparql.term('xyz','literal',datatype),
                                    sparql.term('abc','literal',datatype)) IS NOT NULL)
       THEN RAISE EXCEPTION 'different unresolved literal values remain errors'; END IF;
    FOREACH zone IN ARRAY ARRAY['UTC','Europe/Berlin','America/Los_Angeles','Pacific/Chatham'] LOOP
        PERFORM set_config('TimeZone',zone,true);
        IF EXISTS (SELECT FROM (VALUES
            ('2010-06-21T11:28:01Z',2010,6,21,11,28,1),
            ('2010-12-21T15:38:02-08:00',2010,12,21,15,38,2),
            ('2008-06-20T23:59:00Z',2008,6,20,23,59,0),
            ('2011-02-01T01:02:03',2011,2,1,1,2,3),
            ('2000-12-31T23:59:58.125+14:00',2000,12,31,23,59,58.125)) v(value,y,m,d,h,mi,s)
            CROSS JOIN LATERAL (SELECT sparql.term(value,'literal',x || 'dateTime') AS term) a
            WHERE (sparql.year(term)->>'value')::numeric IS DISTINCT FROM y
               OR (sparql.month(term)->>'value')::numeric IS DISTINCT FROM m
               OR (sparql.day(term)->>'value')::numeric IS DISTINCT FROM d
               OR (sparql.hours(term)->>'value')::numeric IS DISTINCT FROM h
               OR (sparql.minutes(term)->>'value')::numeric IS DISTINCT FROM mi
               OR (sparql.seconds(term)->>'value')::numeric IS DISTINCT FROM s)
           THEN RAISE EXCEPTION 'lexical date components in session timezone %',zone; END IF;
    END LOOP;
    PERFORM set_config('TimeZone',timezone,true);
END $$;
DO $$
DECLARE
    x constant text := 'http://www.w3.org/2001/XMLSchema#';
    raised boolean := false; number double precision; amount numeric; expected text;
BEGIN
    IF sparql.within(10,10) IS DISTINCT FROM 10 THEN RAISE EXCEPTION 'within at the limit'; END IF;
    BEGIN
        PERFORM sparql.within(11,10);
    EXCEPTION WHEN SQLSTATE '54000' THEN
        IF SQLERRM IS DISTINCT FROM 'SPARQL result exceeds 10 bytes' THEN RAISE EXCEPTION 'within message: %',SQLERRM; END IF;
        raised := true;
    END;
    IF NOT raised THEN RAISE EXCEPTION 'within one past the limit'; END IF;
    FOR number,expected IN VALUES (1::double precision,'1'),(125,'125'),(1050,'1050'),(999999.5,'999999.5'),
            (1e6,'1.0E6'),(1e-7,'1.0E-7'),(-3.5e-7,'-3.5E-7'),(0,'0'),('-0','-0'),
            ('Infinity','INF'),('-Infinity','-INF'),('NaN','NaN') LOOP
        IF sparql.format_double(number) IS DISTINCT FROM expected THEN
            RAISE EXCEPTION 'format_double(%) is %, expected %',number,sparql.format_double(number),expected; END IF;
    END LOOP;
    FOR amount,expected IN VALUES (1::numeric,'1'),(0.5,'0.5'),(2.25,'2.25'),(-0.125,'-0.125'),(2.500,'2.5') LOOP
        IF sparql.format_decimal(amount) IS DISTINCT FROM expected THEN
            RAISE EXCEPTION 'format_decimal(%) is %, expected %',amount,sparql.format_decimal(amount),expected; END IF;
    END LOOP;
    -- A SUM or AVG and the arithmetic of the same value are spelled alike.
    IF sparql.aggregate('SUM',ARRAY[sparql.term('1.0E2','literal',x || 'double'),
            sparql.term('2.5E1','literal',x || 'double')],false,' ')
            IS DISTINCT FROM sparql.term('125','literal',x || 'double')
       OR sparql.arithmetic('+',sparql.term('1.0E2','literal',x || 'double'),sparql.term('2.5E1','literal',x || 'double'))
            IS DISTINCT FROM sparql.term('125','literal',x || 'double')
       THEN RAISE EXCEPTION 'double aggregate and arithmetic spelling'; END IF;
    IF sparql.aggregate('AVG',ARRAY[sparql.number(1,'integer'),sparql.number(3,'integer')],false,' ')
            IS DISTINCT FROM sparql.term('2','literal',x || 'decimal')
       OR sparql.arithmetic('/',sparql.number(4,'integer'),sparql.number(2,'integer'))
            IS DISTINCT FROM sparql.term('2','literal',x || 'decimal')
       THEN RAISE EXCEPTION 'decimal aggregate and arithmetic spelling'; END IF;
END $$;
DO $$
DECLARE
    x constant text := 'http://www.w3.org/2001/XMLSchema#';
    one jsonb := sparql.term('1','literal',x || 'integer');
BEGIN
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind) CROSS JOIN (VALUES
        ('1e400','INF'),('-1e400','-INF'),('1e-400','0'),('-1e-400','-0'),('1e999999999','INF'),('0e999999999','0'),
        (repeat('9',400),'INF'),('-0.' || repeat('0',400) || '1','-0'),
        ('0.' || repeat('0',400) || '1E+900','INF'),('1' || repeat('0',400) || 'e-900','0')) v(lexical,expected)
        CROSS JOIN LATERAL (SELECT sparql.term(lexical,'literal',x || kind) AS term) t
        WHERE sparql.numeric(term) IS DISTINCT FROM replace(expected,'INF','Infinity')::numeric
           OR sparql.format_double(sparql.floating(term,kind)) IS DISTINCT FROM expected)
       THEN RAISE EXCEPTION 'a float or double out of range is INF, -INF or a signed zero'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('integer',''),('decimal','0.'),('double','')) v(kind,prefix)
        CROSS JOIN (VALUES (16383),(16384)) b(bytes)
        CROSS JOIN LATERAL (SELECT sparql.term(prefix || repeat('1',bytes-length(prefix)),'literal',x || kind) AS term) t
        WHERE (sparql.numeric(term) IS NULL) IS DISTINCT FROM (bytes>16383))
       THEN RAISE EXCEPTION 'a lexical form longer than 16383 bytes is no number'; END IF;
    IF EXISTS (SELECT FROM (VALUES (ARRAY[one,NULL]),(ARRAY[NULL,one])) v(terms)
        WHERE sparql.aggregate('MIN',terms,false,' ') IS NOT NULL
           OR sparql.aggregate('MAX',terms,false,' ') IS DISTINCT FROM one)
       THEN RAISE EXCEPTION 'MIN and MAX over a partly unbound operand'; END IF;
END $$;
DO $$
DECLARE pattern text; replacement text; found text;
BEGIN
    FOR pattern,replacement,found IN
        SELECT e,'',left(e,2) FROM unnest(string_to_array('\p{Lu} \P{L} \i \c \C \b \B \y \m \u0041 \x41 \0 \é',' ')) e
        UNION ALL VALUES ('[a\p{Lu}]','','\p'),('[a\b]','','\b'),('[a-z-[aeiou]]','','[a-z-[aeiou]]'),
            ('[a-z&&[^b]]','','[a-z&&[^b]]'),
            ('(a)\10','','\10'),('a','$10','$10'),('a','$01','$01'),('''(.*?)''(.*)','','*` and `*?'),
            ('a+?|b','','|` and `+?'),('(x|y)a+?','','|` and `+?'),('a{,3}','','{,3}'),('a}','','}'),('a]','',']'),
            ('[a','','[a'),('\\(?i)','','(?i'),('a{256}','','{256}'),('a{0,00300}','','{0,00300}'),
            ('((a{1,255}){1,255}){1,255}','','((a{1,255}){1,255}){1,255}'),
            ('(?i)a','','(?i'),('***=a','','*'),('[]a]','','[]a]'),('[^]a]','','[^]a]'),('[[=a=]]','','[[=a=]]'),
            ('[[:<:]]a','','[[:<:]]a') LOOP
        BEGIN
            PERFORM sparql.replace(sparql.term('a'),sparql.term(pattern),sparql.term(replacement));
        EXCEPTION WHEN feature_not_supported THEN
            IF strpos(SQLERRM,'found `' || found || '`')=0
               THEN RAISE EXCEPTION 'regex message: %',SQLERRM; END IF;
            CONTINUE;
        END;
        RAISE EXCEPTION 'not rejected: % %',pattern,replacement;
    END LOOP;
END $$;
DO $$
DECLARE
    x constant text := 'http://www.w3.org/2001/XMLSchema#';
    one jsonb := sparql.term('1','literal',x || 'double');
    inf jsonb := sparql.term('INF','literal',x || 'double');
BEGIN
    -- The last two are doubles halfway between a float and INF or 0, whose even neighbour is
    -- INF and 0; the digits that spell them lie on the float's side.
    IF EXISTS (SELECT FROM (VALUES ('1e300','double','INF'),('-1e300','double','-INF'),
        ('1e-300','double','0'),('-1e-300','double','-0'),
        (repeat('9',400),'integer','INF'),('-' || repeat('9',400),'integer','-INF'),
        ('0.' || repeat('0',400) || '1','decimal','0'),('-0.' || repeat('0',400) || '1','decimal','-0'),
        ('3.4028235677973366e38','double','INF'),('7.00649232162408536e-46','double','0')) v(lexical,kind,expected)
        WHERE sparql.cast(x || 'float',sparql.term(lexical,'literal',x || kind))
              IS DISTINCT FROM sparql.term(expected,'literal',x || 'float'))
       THEN RAISE EXCEPTION 'a cast out of a float''s range is INF, -INF or a signed zero'; END IF;
    IF EXISTS (SELECT FROM (SELECT sparql.term(repeat('9',400),'literal',x || 'integer')) v(term)
        WHERE sparql.equal(term,inf) IS DISTINCT FROM true OR sparql.relation('>',term,one) IS DISTINCT FROM true
           OR sparql.arithmetic('+',term,one) IS DISTINCT FROM inf)
       THEN RAISE EXCEPTION 'an integer out of a double''s range is compared and added as INF'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('float'),('double')) d(kind)
        CROSS JOIN LATERAL (SELECT sparql.term('+INF','literal',x || kind) AS term) t
        WHERE sparql.numeric(term) IS DISTINCT FROM 'Infinity'
           OR sparql.equal(term,inf) IS DISTINCT FROM true
           OR sparql.cast(x || kind,sparql.term('+INF')) IS DISTINCT FROM sparql.term('INF','literal',x || kind))
       THEN RAISE EXCEPTION '+INF is INF'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('2024-12-31T23:59:59.9999999Z','2024-12-31 23:59:59.999999Z'),
        ('2024-12-31T23:59:59.999999999','2024-12-31 23:59:59.999999Z'),
        ('2024-06-30T10:00:00.1234567+02:00','2024-06-30 08:00:00.123456Z'),
        ('2024-06-30T10:00:00.123456-02:00','2024-06-30 12:00:00.123456Z'),
        ('2024-06-30T10:00:00.' || repeat('1',300) || 'Z','2024-06-30 10:00:00.111111Z'),
        ('2024-06-30T24:00:00.0000000Z','2024-07-01 00:00:00Z'),('2024-06-30T24:00:00.0000001Z',NULL),
        ('2024-06-30T23:59:60Z',NULL),('2024-06-30T23:59:59Z','2024-06-30 23:59:59Z'),
        ('2024-06-30','2024-06-30 00:00:00Z'),('2024-06-30+02:00','2024-06-29 22:00:00Z'),
        ('2024-06-30-02:00','2024-06-30 02:00:00Z'),('10000-01-01','10000-01-01 00:00:00Z'),
        ('02024-01-01',NULL)) v(lexical,expected)
        WHERE sparql.instant(lexical,x || CASE WHEN lexical LIKE '%T%' THEN 'dateTime' ELSE 'date' END)
              IS DISTINCT FROM expected::timestamptz)
       OR sparql.year(sparql.term('2024-12-31T23:59:59.9999999Z','literal',x || 'dateTime'))
          IS DISTINCT FROM sparql.integer(2024)
       THEN RAISE EXCEPTION 'XSD dates and times as instants'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('dateTime','2024-01-01','2024-03-01T00:00:00Z','2024-01-01T00:00:00Z'),
        ('date','2024-01-01T10:00:00','2024-03-01','2024-01-01T10:00:00Z')) v(kind,lexical,later,same)
        CROSS JOIN LATERAL (SELECT sparql.term(lexical,'literal',x || kind) AS term,
            sparql.term(later,'literal',x || kind) AS other) t
        WHERE sparql.relation('<',term,other) IS NOT NULL OR sparql.relation('>',other,term) IS NOT NULL
           OR sparql.equal(term,sparql.term(same,'literal',x || kind)) IS NOT NULL
           OR sparql.equal(term,term) IS DISTINCT FROM true
           OR sparql.order(term)<sparql.order(other)
           OR sparql.cast(x || 'string',term) IS NOT NULL OR sparql.cast(x || kind,term) IS NOT NULL
           OR sparql.cast(x || kind,sparql.term(lexical)) IS NOT NULL OR sparql.year(term) IS NOT NULL)
       THEN RAISE EXCEPTION 'a date with a time or a dateTime without one is ill-typed'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('7','integer','2','integer','3.5'),('-7','integer','0.5','decimal','-14'),
        ('0.1','decimal','4','byte','0.025'),('1','integer','0','integer',NULL),('1.5','decimal','0.0','decimal',NULL),
        ('0','integer','-0','decimal',NULL)) v(a,akind,b,bkind,expected)
        WHERE sparql.arithmetic('/',sparql.term(a,'literal',x || akind),sparql.term(b,'literal',x || bkind))
              IS DISTINCT FROM sparql.term(expected,'literal',x || 'decimal'))
       THEN RAISE EXCEPTION 'exact division'; END IF;
    IF EXISTS (SELECT FROM (SELECT sparql.term(repeat('9',16383),'literal',x || 'integer')) v(term)
        CROSS JOIN LATERAL (VALUES (sparql.arithmetic('+',term,term)),
            (sparql.aggregate('SUM',ARRAY[term,term],false,NULL))) f(result)
        WHERE result IS DISTINCT FROM sparql.term('1' || repeat('9',16382) || '8','literal',x || 'integer'))
       THEN RAISE EXCEPTION 'a sum of 16,383 digits'; END IF;
    IF sparql.strdt(sparql.term('a'),sparql.term('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString','uri')) IS NOT NULL
       OR sparql.strdt(sparql.term('a'),sparql.term('urn:x','uri')) IS DISTINCT FROM sparql.term('a','literal','urn:x')
       THEN RAISE EXCEPTION 'STRDT makes no language-tagged string'; END IF;
    IF EXISTS (SELECT FROM (VALUES ('straße','STRASSE','straße'),('ÉI','ÉI','éi'),('ΑΣ','ΑΣ','ας')) v(value,upper,lower)
        WHERE sparql.ucase(sparql.term(value)) IS DISTINCT FROM sparql.term(upper)
           OR sparql.lcase(sparql.term(value)) IS DISTINCT FROM sparql.term(lower))
       THEN RAISE EXCEPTION 'full case mapping'; END IF;
    IF sparql.encode_for_uri(sparql.term('')) IS DISTINCT FROM sparql.term('')
       OR sparql.encode_for_uri(sparql.term(E'\u0001\u007F aZ09-._~!*()''@[`{/é𝄞')) IS DISTINCT FROM
          sparql.term('%01%7F%20aZ09-._~%21%2A%28%29%27%40%5B%60%7B%2F%C3%A9%F0%9D%84%9E')
       THEN RAISE EXCEPTION 'URI bytes'; END IF;
END $$;
ROLLBACK;
