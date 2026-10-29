require src/lib/test.f
require src/lib/xml.f
require src/lib/test/guard-page.f
require src/lib/byte-buffer.f

package XML-TEST
using XML

$400 constant TEXT-CAP
create TEXT-BUF TEXT-CAP allot
create OTHER-BUF TEXT-CAP allot
3 SPAN-BUFFER: INPUT-CHUNK
PTR-VARIABLE INPUT-SOURCE
variable INPUT-LEN
variable INPUT-POS
variable INPUT-LIMIT
variable INPUT-FAIL
variable INPUT-OFF
variable INPUT-USED
create INPUT-BIG 8200 allot
create DEEP-DOC BUF:HDR-BYTES allot
100000 constant DEEP-LEVELS
4 BUFFER: DEEP-NAME
variable DEEP-BAD
TYPED-VARIABLE RD XML:cursor
TYPED-VARIABLE RD2 XML:cursor

CAST: TOKEN>N ( XML:kind -- n )
CAST: CURSOR>BYTES ( ptr XML:cursor -- ptr u8 )

: OPEN ( ptr u8 n -- XML:reader )
   {: source size:n :}
   RD source size INIT ;

: NEXT= ( XML:reader XML:kind -- XML:reader )
   TOKEN>N {: expected:n :}
   NEXT TOKEN>N expected T= ;

: RAW= ( XML:reader n n -- XML:reader )
   {: start:n size:n :}
   RAW LEN>N size T= OFF>N start T= ;

: NAME= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   NAME$ expected size T$= ;

: LOCAL= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   LOCAL$ expected size T$= ;

: URI= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   TEXT-BUF TEXT-CAP URI TEXT-BUF swap expected size T$= ;

: PREFIX= ( XML:reader ptr u8 n ptr u8 n -- XML:reader )
   {: prefix prefix-size:n expected size:n :}
   prefix prefix-size T-LABEL
   prefix prefix-size TEXT-BUF TEXT-CAP PREFIX-URI TTRUE
   TEXT-BUF swap expected size T$= ;

: PREFIX-MISSING ( XML:reader ptr u8 n -- XML:reader )
   $5A TEXT-BUF c!
   TEXT-BUF TEXT-CAP PREFIX-URI TFALSE 0 T=
   TEXT-BUF c@ $5A T= ;

: PREFIX-READ ( XML:reader -- XML:reader )
   s" p" TEXT-BUF TEXT-CAP PREFIX-URI 2drop ;

: TEXT= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   TEXT-BUF TEXT-CAP TEXT TEXT-BUF swap expected size T$= ;

: ATTR-NAME= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   ATTR-NAME$ expected size T$= ;

: ATTR-URI= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   TEXT-BUF TEXT-CAP ATTR-URI TEXT-BUF swap expected size T$= ;

: ATTR-TEXT= ( XML:reader ptr u8 n -- XML:reader )
   {: expected size:n :}
   TEXT-BUF TEXT-CAP ATTR-TEXT TEXT-BUF swap expected size T$= ;

: DRAIN ( XML:reader -- XML:reader )
   begin NEXT TOKEN>N XML-KIND:EOF TOKEN>N <> while repeat ;

: INPUT-SET ( ptr u8 n n n -- )
   {: source size:n limit:n fail-at:n :}
   source INPUT-SOURCE ! size INPUT-LEN !
   limit INPUT-LIMIT ! fail-at INPUT-FAIL !
   0 INPUT-POS ! ;

: INPUT-FILL ( ptr n SPAN:span<u8> -- n )
   {: pos:ptr output :}
   pos @ INPUT-FAIL @ >= if -9299 throw then
   INPUT-LEN @ pos @ -
   output SPAN:LEN min INPUT-LIMIT @ min {: count:n :}
   count 0= if 0 exit then
   INPUT-SOURCE @ pos @ + count output SPAN:COPY
   count pos +! count ;

: INPUT-OPEN ( -- XML:reader )
   RD INPUT-POS ['] INPUT-FILL INPUT-CHUNK INIT-INPUT ;

: INPUT-FAILURE ( -- n )
   INPUT-OPEN ['] DRAIN catch swap XML:CLOSE ;

: INPUT-OVER-COUNT ( ptr n SPAN:span<u8> -- n )
   2drop 4 ;

: INPUT-OVER-COUNT-RC ( -- n )
   RD INPUT-POS ['] INPUT-OVER-COUNT INPUT-CHUNK INIT-INPUT
   ['] DRAIN catch swap XML:CLOSE ;

: INPUT-TEXT-ALIASED ( XML:reader -- XML:reader )
   INPUT-CHUNK SPAN:$ TEXT drop ;

: INPUT-BAD ( ptr u8 n n -- )
   {: source size:n expected:n :}
   source size 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE expected T= ;

\ The reader is opened INSIDE the caught body, as BAD-INIT below does: `catch`
\ restores the DEPTH of both stacks and never their contents, so
\ a reader opened before the catch comes back `stale<XML:reader>` and a linear
\ cell can be neither read nor dropped. The body closes the reader its failed
\ NEXT leaves, as the contract requires, and rethrows that failure.
: BAD ( ptr u8 n n -- )
   {: expected:n :}
   [: 2dup OPEN ['] DRAIN catch swap XML:CLOSE throw ;] catch expected T=
   2drop ;

: BAD-INIT ( ptr u8 n n -- )
   {: expected:n :}
   [: 2dup OPEN XML:CLOSE ;] catch expected T=
   2drop ;

: EMPTY-ELEMENT ( -- )
   s" <a/>" OPEN
   XML-KIND:START NEXT= 0 4 RAW= s" a" NAME=
   XML:DEPTH 1 T=
   ATTR-NEXT TFALSE
   XML-KIND:END NEXT= 4 0 RAW= s" a" NAME=
   XML:DEPTH 1 T=
   XML-KIND:EOF NEXT= 4 0 RAW= XML:DEPTH 0 T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: NAMESPACE-SCOPES ( -- )
   s\" <a xmlns='urn:outer' xmlns:p='urn:p' plain='x' p:id='1'><p:b xmlns:p='urn:inner'><c xmlns=''/></p:b></a>" OPEN
   XML-KIND:START NEXT= s" urn:outer" URI=
   ATTR-NEXT TTRUE s" xmlns" ATTR-NAME=
   s" http://www.w3.org/2000/xmlns/" ATTR-URI=
   ATTR-NEXT TTRUE s" xmlns:p" ATTR-NAME=
   ATTR-NEXT TTRUE s" plain" ATTR-NAME= s" " ATTR-URI=
   ATTR-NEXT TTRUE s" p:id" ATTR-NAME= s" urn:p" ATTR-URI=
   ATTR-LOCAL$ s" id" T$=
   ATTR-NEXT TFALSE
   ATTR-RESET ATTR-NEXT TTRUE s" xmlns" ATTR-NAME=
   XML-KIND:START NEXT= s" p:b" NAME= s" b" LOCAL= s" urn:inner" URI=
   XML-KIND:START NEXT= s" " URI=
   XML-KIND:END NEXT= s" " URI=
   XML-KIND:END NEXT= s" urn:inner" URI=
   XML-KIND:END NEXT= s" urn:outer" URI=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: NAMESPACE-ENTITIES ( -- )
   s\" <p:a p:x='1' xmlns:p='urn:&#x61;' xml:lang='en'/>" OPEN
   XML-KIND:START NEXT= s" urn:a" URI=
   ATTR-NEXT TTRUE s" urn:a" ATTR-URI=
   ATTR-NEXT TTRUE
   ATTR-NEXT TTRUE s" http://www.w3.org/XML/1998/namespace" ATTR-URI=
   XML:CLOSE ;

: PREFIX-VALUES ( -- )
   s" <a xmlns='urn:root' xmlns:req='urn:one' xmlns:alias='urn:one' xmlns:mc='urn:mc'><mc:Choice Requires='req alias'><b xmlns:req='urn:two' xmlns=''/></mc:Choice></a>" OPEN
   XML-KIND:START NEXT=
   s" req" s" urn:one" PREFIX=
   s" alias" s" urn:one" PREFIX=
   s" " s" urn:root" PREFIX=
   s" xml" s" http://www.w3.org/XML/1998/namespace" PREFIX=
   s" xmlns" PREFIX-MISSING
   s" absent" PREFIX-MISSING
   XML-KIND:START NEXT=
   ATTR-NEXT TTRUE s" Requires" ATTR-NAME=
   s" req alias" ATTR-TEXT=
   s" req" s" urn:one" PREFIX=
   s" alias" s" urn:one" PREFIX=
   s" absent" PREFIX-MISSING s" Requires" ATTR-NAME=
   XML-KIND:START NEXT=
   s" req" s" urn:two" PREFIX=
   s" alias" s" urn:one" PREFIX=
   s" " s" " PREFIX=
   XML-KIND:END NEXT= s" req" s" urn:two" PREFIX=
   XML-KIND:END NEXT= s" req" s" urn:one" PREFIX=
   s" " s" urn:root" PREFIX=
   XML-KIND:END NEXT= s" req" s" urn:one" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: PREFIX-ENTITY-AND-SCOPE ( -- )
   s\" <?go?><a xmlns:p='urn:&#97;\r\nz'><!--inside--><?go in?><b/></a><!--outside-->" OPEN
   XML-KIND:PI NEXT=
   ['] PREFIX-READ catch E-STATE T=
   XML-KIND:START NEXT=
   s" p" s" urn:a z" PREFIX=
   XML-KIND:COMMENT NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:PI NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:START NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:END NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:END NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:COMMENT NEXT= ['] PREFIX-READ catch E-STATE T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: SCALARS-AND-OFFSETS ( -- )
   s\" \xEF\xBB\xBF<é>😀&amp;&#x1F680;\r\nZ&#13;</é>" OPEN
   XML-KIND:START NEXT= 3 4 RAW= s" é" NAME=
   XML-KIND:TEXT NEXT= 7 26 RAW=
   s\" 😀&🚀\nZ\r" TEXT=
   XML-KIND:END NEXT= 33 5 RAW=
   XML-KIND:EOF NEXT= 38 0 RAW=
   XML:CLOSE ;

: ATTRIBUTES-AND-SPANS ( -- )
   s\" <a q = 'x\r\ny\t&amp;&#9;&#13;&quot;&apos;&lt;&gt;'/>" OPEN
   XML-KIND:START NEXT=
   ATTR-NEXT TTRUE
   ATTR-RAW LEN>N 45 T= OFF>N 3 T=
   s\" x y &\t\r\q'<>" ATTR-TEXT=
   ATTR-VALUE$ s\" x\r\ny\t&amp;&#9;&#13;&quot;&apos;&lt;&gt;" T$=
   XML:CLOSE ;

: MARKUP-TEXT ( -- )
   s\" <?xml version='1.0' encoding='uTf-8' standalone='yes'?><!--a&amp;--><a><![CDATA[]]><![CDATA[x&y\r\nz]]><?go a&b?></a>" OPEN
   XML-KIND:PI NEXT= s" xml" NAME=
   XML-KIND:COMMENT NEXT= s" a&amp;" TEXT=
   XML-KIND:START NEXT=
   XML-KIND:CDATA NEXT= s" " TEXT=
   XML-KIND:CDATA NEXT= s\" x&y\nz" TEXT=
   XML-KIND:PI NEXT= s" go" NAME= s" a&b" TEXT=
   XML-KIND:END NEXT=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: UNICODE-NAMES ( -- )
   s" <名:根 xmlns:名='urn:名' 名:值='é'><𐀀/></名:根>" OPEN
   XML-KIND:START NEXT= s" 根" LOCAL= s" urn:名" URI=
   XML-KIND:START NEXT= s" 𐀀" NAME=
   DRAIN XML:CLOSE ;

: MALFORMED-INPUT ( -- )
   s" " E-MALFORMED BAD
   s" <a>" E-TRUNCATED BAD
   s" <a" E-TRUNCATED BAD
   s" <a></b>" E-MALFORMED BAD
   s" <a/><b/>" E-MALFORMED BAD
   s" x<a/>" E-MALFORMED BAD
   s" <a/>x" E-MALFORMED BAD
   s" <a>]]></a>" E-MALFORMED BAD
   s" <!--a--b--><a/>" E-MALFORMED BAD
   s" <!--a" E-TRUNCATED BAD
   s" <![CDATA[x]]><a/>" E-MALFORMED BAD
   s" <a><![CDATA[x" E-TRUNCATED BAD
   s" <?go" E-TRUNCATED BAD
   s" <?go?><a/>" OPEN DRAIN XML:CLOSE
   s" <a q='x'v='y'/>" E-MALFORMED BAD
   s" <a q='<x'/>" E-MALFORMED BAD
   s" <a q='x/>" E-TRUNCATED BAD
   s" <a></a x='1'>" E-MALFORMED BAD
   s" <!DOCTYPE a [<!ENTITY x SYSTEM 'file:///no'>]><a/>" E-DTD BAD ;

: BAD-ENTITIES ( -- )
   s" <a>&unknown;</a>" E-ENTITY BAD
   s" <a>&amp</a>" E-ENTITY BAD
   s" <a>&#;</a>" E-ENTITY BAD
   s" <a>&#x;</a>" E-ENTITY BAD
   s" <a>&#x110000;</a>" E-ENTITY BAD
   s" <a>&#99999999999999999999;</a>" E-ENTITY BAD
   s" <a>&#xD800;</a>" E-SCALAR BAD
   s" <a>&#0;</a>" E-SCALAR BAD
   s" <a>&#12;</a>" E-SCALAR BAD
   s" <a>&#X20;</a>" E-ENTITY BAD ;

: BAD-NAMESPACES ( -- )
   s" <p:a/>" E-NAMESPACE BAD
   s" <a p:q='x'/>" E-NAMESPACE BAD
   s" <:a/>" E-NAMESPACE BAD
   s" <p:/>" E-NAMESPACE BAD
   s" <p:1/>" E-NAMESPACE BAD
   s" <p:a:b/>" E-NAMESPACE BAD
   s" <a q='1' q='2'/>" E-NAMESPACE BAD
   s" <a xmlns:p='u' xmlns:p='v'/>" E-NAMESPACE BAD
   s" <a xmlns:p='u0' xmlns:q='u&#48;' p:x='1' q:x='2'/>" E-NAMESPACE BAD
   s" <a xmlns:p=''/>" E-NAMESPACE BAD
   s" <a xmlns:xml='other'/>" E-NAMESPACE BAD
   s" <a xmlns:xmlns='u'/>" E-NAMESPACE BAD
   s" <a xmlns='http://www.w3.org/XML/1998/namespace'/>" E-NAMESPACE BAD
   s" <a xmlns:p='http://www.w3.org/2000/xmlns/'/>" E-NAMESPACE BAD ;

\ Each start tag reuses the attribute table: a sibling with fewer or more
\ attributes than the one before still finds its own duplicates, and only them.
: SIBLING-ATTRIBUTES ( -- )
   s" <r><a x='1' y='2' z='3'/><b z='1' w='2'/></r>" OPEN DRAIN XML:CLOSE
   s" <r><a x='1' y='2' z='3'/><b y='1' y='2'/></r>" E-NAMESPACE BAD
   s" <r><a x='1'/><b y='1' z='2' y='3'/></r>" E-NAMESPACE BAD
   s" <r xmlns:p='u' xmlns:q='u'><a p:x='1' q:y='2' z='3'/><b z='1' p:x='2' q:x='3'/></r>"
      E-NAMESPACE BAD ;

\ A child's rebinding is seen by its descendants and ends with it, whether it
\ closes with an end tag or self-closes.
: NAMESPACE-SHADOWING ( -- )
   s" <a xmlns:p='urn:outer'><b xmlns:p='urn:inner'><p:c p:x='1'/></b><p:m/><d xmlns:p='urn:self' p:x='2'/><p:e p:x='3'/></a>" OPEN
   XML-KIND:START NEXT= s" p" s" urn:outer" PREFIX=
   XML-KIND:START NEXT= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" urn:inner" URI= ATTR-NEXT TTRUE s" urn:inner" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" b" NAME= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" urn:outer" URI= s" p" s" urn:outer" PREFIX=
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" p" s" urn:self" PREFIX=
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE s" urn:self" ATTR-URI=
   XML-KIND:END NEXT= s" d" NAME=
   XML-KIND:START NEXT= s" urn:outer" URI= s" p" s" urn:outer" PREFIX=
   ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" a" NAME= s" p" s" urn:outer" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: BAD-ENCODINGS ( -- )
   s\" \xFF\xFE<\z" E-ENCODING BAD-INIT
   s\" \xFE\xFF\z<" E-ENCODING BAD-INIT
   s\" <\za\z/\z>\z" E-ENCODING BAD-INIT
   s\" \z<\za\z/\z>" E-ENCODING BAD-INIT
   s" <?xml version='1.0' encoding='UTF-16'?><a/>" E-ENCODING BAD
   s" <?xml version='1.1'?><a/>" E-ENCODING BAD
   s" <?XML version='1.0'?><a/>" E-MALFORMED BAD
   s"  <?xml version='1.0'?><a/>" E-MALFORMED BAD
   s" <?xml encoding='UTF-8' version='1.0'?><a/>" E-MALFORMED BAD
   s" <?xml version='1.0' standalone='maybe'?><a/>" E-MALFORMED BAD
   s\" <a>\xC0\x80</a>" E-UTF8 BAD
   s\" <a>\xED\xA0\x80</a>" E-UTF8 BAD
   s\" <a>\xF4\x90\x80\x80</a>" E-UTF8 BAD
   s\" <a>\z</a>" E-SCALAR BAD ;

: ASCII-WIRE ( -- )
   s\" <?xml version=\q1.0\q encoding='Us-AsCiI'?><a q='&#x4E2D;'>&#x4E2D;</a>"
   OPEN XML-KIND:PI NEXT= XML-KIND:START NEXT=
   ATTR-NEXT TTRUE s" 中" ATTR-TEXT=
   XML-KIND:TEXT NEXT= s" 中" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><!--é--></a>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><![CDATA[é]]></a>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/><?p é?>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>é</a>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>é"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a x='é'/>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><é/>"
      E-ENCODING BAD
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
      E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>é</a>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><!--é--></a>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><![CDATA[é]]></a>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/><?p é?>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><é/>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a q='é'/>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>é"
      E-ENCODING INPUT-BAD
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
      E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>&#x4E2D;</a>"
      1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML-KIND:TEXT NEXT= s" 中" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

\ Every case below reads the reader after the catch, so the handle must SURVIVE
\ the caught failure. A quotation literal leaves it `stale<XML:reader>` - `catch`
\ restores the DEPTH of both stacks and never their contents - and a linear cell
\ can be neither read nor dropped, so each body is a name reached by `[']`: an
\ exceptional edge is not part of a quotation's TYPE, so that route keeps the
\ window typed until callee evidence lets the checker prove the handle intact.
\ ATTR-RESET already has that shape and is ticked by its bare name, the one a
\ `using` import resolves (src/habu/habu2.f C-BTICK).
: NEXT-ONE ( XML:reader -- XML:reader ) NEXT drop ;
: ATTR-NEXT-ONE ( XML:reader -- XML:reader ) ATTR-NEXT drop ;
: NAME-READ ( XML:reader -- XML:reader ) NAME$ 2drop ;
: ATTR-NAME-READ ( XML:reader -- XML:reader ) ATTR-NAME$ 2drop ;
: ATTR-RAW-READ ( XML:reader -- XML:reader ) ATTR-RAW 2drop ;
: ATTR-VALUE-READ ( XML:reader -- XML:reader ) ATTR-VALUE 2drop ;
: ATTR-VALUE$-READ ( XML:reader -- XML:reader ) ATTR-VALUE$ 2drop ;
: CONTENT-READ ( XML:reader -- XML:reader ) CONTENT 2drop ;
: TEXT-FULL ( XML:reader -- XML:reader ) TEXT-BUF TEXT-CAP TEXT drop ;
: TEXT-TINY ( XML:reader -- XML:reader ) TEXT-BUF 1 TEXT drop ;
: TEXT-ALIASED ( XML:reader -- XML:reader ) RAW$ drop TEXT-CAP TEXT drop ;
: TEXT-INTO-RECORD ( XML:reader -- XML:reader ) RD CURSOR>BYTES 16 TEXT drop ;
: URI-TINY ( XML:reader -- XML:reader ) TEXT-BUF 4 URI drop ;
: URI-ALIASED ( XML:reader -- XML:reader ) RAW$ drop TEXT-CAP URI drop ;
: ATTR-URI-TINY ( XML:reader -- XML:reader ) TEXT-BUF 4 ATTR-URI drop ;
: ATTR-URI-ALIASED ( XML:reader -- XML:reader ) RAW$ drop TEXT-CAP ATTR-URI drop ;
: ATTR-TEXT-TINY ( XML:reader -- XML:reader ) TEXT-BUF 1 ATTR-TEXT drop ;
: ATTR-TEXT-ALIASED ( XML:reader -- XML:reader ) RAW$ drop TEXT-CAP ATTR-TEXT drop ;
: PREFIX-TINY ( XML:reader -- XML:reader )
   s" p" TEXT-BUF 2 PREFIX-URI 2drop ;
: PREFIX-ALIASED ( XML:reader -- XML:reader )
   RAW$ drop {: raw :}
   s" p" raw TEXT-CAP PREFIX-URI 2drop ;

: FAILURE-AND-STATE ( -- )
   s" <a><b></a>" OPEN
   ['] DRAIN catch E-MALFORMED T=
   ['] NEXT-ONE catch E-STATE T=
   XML:CLOSE
   s" <a q='x'/>" OPEN
   ['] ATTR-NEXT-ONE catch E-STATE T=
   ['] ATTR-RESET catch E-STATE T=
   ['] NAME-READ catch E-STATE T=
   ['] ATTR-RAW-READ catch E-STATE T=
   ['] ATTR-VALUE-READ catch E-STATE T=
   ['] ATTR-VALUE$-READ catch E-STATE T=
   XML-KIND:START NEXT=
   ['] CONTENT-READ catch E-STATE T=
   ['] TEXT-FULL catch E-STATE T=
   s" a" NAME=
   ['] ATTR-NAME-READ catch E-STATE T=
   ATTR-NEXT TTRUE s" q" ATTR-NAME=
   ATTR-NEXT TFALSE
   ['] ATTR-NAME-READ catch E-STATE T=
   XML:CLOSE ;

\ A namespace error once bindings exist fails the reader. A new reader starts
\ with none of them.
: NAMESPACE-FAILURE ( -- )
   s" <a xmlns:p='urn:stale'><b xmlns:q='urn:q' q:x='1' x='2' x='3'/></a>" OPEN
   XML-KIND:START NEXT=
   ['] NEXT-ONE catch E-NAMESPACE T=
   ['] PREFIX-READ catch E-STATE T=
   ['] NAME-READ catch E-STATE T=
   ['] NEXT-ONE catch E-STATE T=
   XML:CLOSE
   s" <c xmlns:r='urn:r'><d/></c>" OPEN
   XML-KIND:START NEXT=
   s" p" PREFIX-MISSING s" q" PREFIX-MISSING s" r" s" urn:r" PREFIX=
   XML-KIND:START NEXT= s" p" PREFIX-MISSING s" r" s" urn:r" PREFIX=
   XML-KIND:END NEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: NO-PARTIAL-DECODE ( -- )
   $5A TEXT-BUF c!
   s" <a>long&amp;text</a>" OPEN
   NEXT drop NEXT drop
   ['] TEXT-TINY catch E-CAPACITY T=
   TEXT-BUF c@ $5A T=
   s" long&text" TEXT=
   ['] TEXT-ALIASED catch E-ALIAS T=
   s" long&text" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

\ The reader writes its record on every event, so neither the source nor a
\ decoder's output may overlap it.
: INIT-INTO-RECORD ( -- ) RD RD CURSOR>BYTES 16 INIT XML:CLOSE ;

: INIT-RECORD-ALIAS ( -- )
   [: INIT-INTO-RECORD ;] E-ALIAS TTHROWSQ ;

: TEXT-RECORD-ALIAS ( -- )
   s" <a>long&amp;text</a>" OPEN
   NEXT drop NEXT drop
   ['] TEXT-INTO-RECORD catch E-ALIAS T=
   s" long&text" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: URI-CAPACITY-RECOVERY ( -- )
   $5A TEXT-BUF c!
   s" <p:a xmlns:p='urn:&#97;'/>" OPEN NEXT drop
   ['] URI-TINY catch E-CAPACITY T=
   TEXT-BUF c@ $5A T=
   s" p:a" NAME= s" urn:a" URI=
   ['] URI-ALIASED catch E-ALIAS T=
   s" urn:a" URI=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: PREFIX-RECOVERY ( -- )
   s" <a xmlns:p='urn:&#97;'/>" OPEN
   ['] PREFIX-READ catch E-STATE T=
   XML-KIND:START NEXT=
   s" " s" " PREFIX=
   $5A TEXT-BUF c!
   ['] PREFIX-TINY catch E-CAPACITY T=
   TEXT-BUF c@ $5A T=
   s" p" s" urn:a" PREFIX=
   ['] PREFIX-ALIASED catch E-ALIAS T=
   s" p" s" urn:a" PREFIX=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   ['] PREFIX-READ catch E-STATE T=
   XML:CLOSE
   s" <a><b></a>" OPEN
   ['] DRAIN catch E-MALFORMED T=
   ['] PREFIX-READ catch E-STATE T=
   XML:CLOSE ;

: PREFIX-READERS ( -- )
   RD s" <a xmlns:p='urn:first'/>" INIT
   RD2 s" <b xmlns:p='urn:second'/>" INIT
   XML-KIND:START NEXT= s" p" s" urn:second" PREFIX=
   swap XML-KIND:START NEXT= s" p" s" urn:first" PREFIX=
   swap s" p" s" urn:second" PREFIX=
   XML:CLOSE XML:CLOSE ;

\ A reader's first table mapping is one page. Freeing a page and mapping one
\ again lands at the same address only when nothing took the hole, so a body
\ that leaves its first table mapped moves the second probe elsewhere. This
\ relies on the kernel's first-fit reuse of a freed mapping, as
\ src/lib/json-read-test.f JRT-RELEASES does.
4096 constant PROBE-BYTES

: PROBE ( -- SPAN:span<u8> )
   PROBE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;

: RELEASES ( [ -- ] -- )
   PROBE {: hole :}
   hole SPAN:$ drop {: at:ptr :}
   hole MEM:FREE-SPAN
   execute
   PROBE {: again :}
   again SPAN:$ drop at = TTRUE
   again MEM:FREE-SPAN ;

77 constant CALLER-CODE

: ALWAYS ( -- bool ) 0 0= ;

: CALLER-FAIL ( XML:reader -- XML:reader )
   ALWAYS if CALLER-CODE throw then ;

\ The caller's own throw abandons a live reader, so record RD keeps its tables
\ and, for an input reader, its stream.
: ABANDON ( -- )
   s" <a><b/></a>" OPEN
   XML-KIND:START NEXT= XML-KIND:START NEXT=
   CALLER-FAIL
   XML:CLOSE ;

: ABANDON-INPUT ( -- )
   s" <a><b/></a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   CALLER-FAIL
   XML:CLOSE ;

\ DISPOSE releases what each abandoned reader left; a second does nothing.
: ABANDON-DISPOSE ( -- )
   [: ABANDON ;] CALLER-CODE TTHROWSQ
   RD XML:DISPOSE RD XML:DISPOSE
   [: ABANDON-INPUT ;] CALLER-CODE TTHROWSQ
   RD XML:DISPOSE RD XML:DISPOSE ;

\ INIT on a record that still holds tables releases them before it reads.
: ABANDON-REINIT ( -- )
   [: ABANDON-INPUT ;] CALLER-CODE TTHROWSQ
   s" <c/>" OPEN
   XML-KIND:START NEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE ;

: ABANDONED-RECORD ( -- )
   s" DISPOSE releases what a caller's throw abandoned" T-LABEL
   [: ABANDON-DISPOSE ;] RELEASES
   s" a record holding a leftover reads again and nothing stays mapped" T-LABEL
   [: ABANDON-REINIT ;] RELEASES ;

: ATTRIBUTE-CAPACITY-RECOVERY ( -- )
   s" <p:a xmlns:p='urn:&#97;' p:q='long&amp;text'/>" OPEN NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   $5A TEXT-BUF c!
   ['] ATTR-URI-TINY catch E-CAPACITY T=
   TEXT-BUF c@ $5A T=
   s" p:q" ATTR-NAME= s" urn:a" ATTR-URI=
   ['] ATTR-URI-ALIASED catch E-ALIAS T=
   s" urn:a" ATTR-URI=
   $5A TEXT-BUF c!
   ['] ATTR-TEXT-TINY catch E-CAPACITY T=
   TEXT-BUF c@ $5A T=
   s" p:q" ATTR-NAME= s" long&text" ATTR-TEXT=
   ['] ATTR-TEXT-ALIASED catch E-ALIAS T=
   s" long&text" ATTR-TEXT=
   ATTR-VALUE$ s" long&amp;text" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

\ One tag of WIDE-PREFIXES declarations and as many attributes p0:a, p1:a, ...
\ URI i has URI-SCALARS scalars, U+0061 or U+20061 by the bits of i. The two
\ differ only in bit 17, so under an unkeyed multiplicative hash such as FNV
\ every such URI, and every expanded name over it, shares the low 17 bits and
\ all the attributes fall in one bucket. The duplicate checks and prefix
\ lookups must stay linear in the tag, or this row runs out of its CPU budget.
\ A duplicate raw name, or an expanded name whose URI is spelled with character
\ references, placed last must still be found among them.
15 constant URI-SCALARS
1 URI-SCALARS lshift constant WIDE-PREFIXES
WIDE-PREFIXES 1- constant WIDE-LAST
$300000 constant WIDE-DOC-CAP
PTR-VARIABLE WIDE-DOC
variable WIDE-LEN
variable URI-LEN

: WIDE+ ( ptr u8 n -- )
   {: source size:n :}
   source WIDE-DOC @ WIDE-LEN @ + size >LEN BYTE-COPY-LEN
   size WIDE-LEN +! ;

: WIDE-N+ ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod $30 + WIDE-DOC @ WIDE-LEN @ + c!
   1 WIDE-LEN +! ;

: URI+ ( ptr u8 n -- )
   {: source size:n :}
   source OTHER-BUF URI-LEN @ + size >LEN BYTE-COPY-LEN
   size URI-LEN +! ;

\ Emit URI index, its U+20061 scalars as UTF-8 or as character references.
: WIDE-URI ( n bool [ ptr u8 n -- ] -- )
   {: index:n refs:bool emit :}
   URI-SCALARS 0 ?do
      index i rshift 1 and 0= if
         s" a" emit execute
      else
         refs if s" &#x20061;" else s\" \xF0\xA0\x81\xA1" then emit execute
      then
   loop ;

: EXPECTED-URI$ ( n -- ptr u8 n )
   0 URI-LEN !
   false [: URI+ ;] WIDE-URI
   OTHER-BUF URI-LEN @ ;

: WIDE-BUILD ( ptr u8 n -- )
   {: tail size:n :}
   0 WIDE-LEN !
   s" <r" WIDE+
   WIDE-PREFIXES 0 ?do
      s"  xmlns:p" WIDE+ i WIDE-N+ s" ='" WIDE+
      i false [: WIDE+ ;] WIDE-URI s" '" WIDE+
   loop
   WIDE-PREFIXES 0 ?do s"  p" WIDE+ i WIDE-N+ s" :a='1'" WIDE+ loop
   tail size WIDE+
   s" />" WIDE+ ;

: WIDE-OPEN ( -- XML:reader ) RD WIDE-DOC @ WIDE-LEN @ INIT ;

: WIDE-BAD ( ptr u8 n -- )
   WIDE-BUILD
   [: WIDE-OPEN ['] DRAIN catch swap XML:CLOSE throw ;] catch E-NAMESPACE T= ;

: WIDE-ELEMENT ( -- )
   WIDE-DOC-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop WIDE-DOC !
   s" " WIDE-BUILD
   WIDE-OPEN XML-KIND:START NEXT=
   s" p0" s" aaaaaaaaaaaaaaa" PREFIX=
   s" p32767" WIDE-LAST EXPECTED-URI$ PREFIX=
   WIDE-PREFIXES 2 * 0 ?do ATTR-NEXT drop loop
   s" p32767:a" ATTR-NAME= WIDE-LAST EXPECTED-URI$ ATTR-URI=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   s"  p0:a='2'" WIDE-BAD
   0 URI-LEN !
   s"  xmlns:q='" URI+ WIDE-LAST true [: URI+ ;] WIDE-URI s" ' q:a='2'" URI+
   OTHER-BUF URI-LEN @ WIDE-BAD
   WIDE-DOC @ WIDE-DOC-CAP MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: ESCAPING ( -- )
   s\" A<&>\r\n\q'😀" TEXT-BUF TEXT-CAP ESCAPE-TEXT
   TEXT-BUF swap s\" A&lt;&amp;&gt;&#13;\n\q'😀" T$=
   s\" A<&>\r\n\t\q'😀" TEXT-BUF TEXT-CAP ESCAPE-ATTR
   TEXT-BUF swap s" A&lt;&amp;&gt;&#13;&#10;&#9;&quot;&apos;😀" T$=
   s" " TEXT-BUF 0 ESCAPE-TEXT 0 T=
   [: s" &" TEXT-BUF 4 ESCAPE-TEXT drop ;] E-CAPACITY TTHROWSQ
   \ Every scalar escapes to at least its own width, so a source longer than the
   \ destination is refused before it is read; this one ends at an inaccessible
   \ page one byte short of the length it claims.
   [: 4 [char] a GUARD-PAGE:TAIL 5 TEXT-BUF 4 ESCAPE-TEXT drop ;] E-CAPACITY TTHROWSQ
   [: 4 [char] a GUARD-PAGE:TAIL 5 TEXT-BUF 4 ESCAPE-ATTR drop ;] E-CAPACITY TTHROWSQ
   [: s\" \z" TEXT-BUF TEXT-CAP ESCAPE-TEXT drop ;] E-SCALAR TTHROWSQ ;

: CHECKED-OWNERSHIP ( -- )
   s" XML-TEST-DUP ( XML:reader -- XML:reader XML:reader ) dup" CHECK! 0 T=
   s" XML-TEST-DROP ( XML:reader -- ) drop" CHECK! 0 T=
   s" XML-TEST-OFFSET ( XML:reader -- XML:reader len off ) XML:RAW" CHECK! 0 T= ;

: INPUT-BOUNDARIES ( -- )
   s\" \xEF\xBB\xBF<?xml version='1.0'?><a xmlns:p='u'><p:b q='&amp;'>Aé&amp;Z</p:b><c/></a> "
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:PI NEXT= 3 21 RAW=
   XML-KIND:START NEXT= 24 15 RAW=
   XML-KIND:START NEXT= 39 15 RAW= s" p:b" NAME= s" b" LOCAL=
   s" u" URI=
   ATTR-NEXT TTRUE
   ATTR-RAW {: attr-off:off attr-len:len :}
   attr-off OFF>N 44 T= attr-len LEN>N 9 T=
   ATTR-VALUE {: value-off:off value-len:len :}
   value-off OFF>N 47 T= value-len LEN>N 5 T=
   s" &" ATTR-TEXT=
   54 INPUT-OFF ! 0 INPUT-USED !
   begin NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      RAW {: start:off size:len :}
      start OFF>N INPUT-OFF @ T=
      TEXT-BUF INPUT-USED @ + TEXT-CAP INPUT-USED @ - TEXT
         {: count:n :}
      size LEN>N INPUT-OFF +!
      count INPUT-USED +!
   repeat
   INPUT-OFF @ 63 T=
   TEXT-BUF INPUT-USED @ s" Aé&Z" T$=
   RAW {: end-off:off end-len:len :}
   end-off OFF>N 63 T= end-len LEN>N 6 T=
   s" p" s" u" PREFIX=
   XML-KIND:START NEXT= 69 4 RAW=
   XML-KIND:END NEXT= 73 0 RAW=
   XML-KIND:END NEXT= 73 4 RAW=
   XML-KIND:TEXT NEXT= 77 1 RAW= s"  " TEXT=
   XML-KIND:EOF NEXT= 78 0 RAW=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-MARKUP ( -- )
   s" <a><!--long--><![CDATA[é&]]><?go q?></a>"
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   XML-KIND:COMMENT NEXT= s" long" TEXT=
   XML-KIND:CDATA NEXT= s" é&" TEXT=
   XML-KIND:PI NEXT= s" q" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-NEWLINE ( -- )
   S\" <a>x\r\ny</a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   0 INPUT-USED !
   begin NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      TEXT-BUF INPUT-USED @ + TEXT-CAP INPUT-USED @ - TEXT
      INPUT-USED +!
   repeat
   TEXT-BUF INPUT-USED @ S\" x\ny" T$=
   KIND TOKEN>N XML-KIND:END TOKEN>N T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-ALIAS-CHECK ( -- )
   s" <a>x</a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   ['] INPUT-TEXT-ALIASED catch E-ALIAS T=
   s" x" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-LARGE-TEXT ( -- )
   s" <r>" drop INPUT-BIG 3 BYTE-COPY
   8192 0 ?do [char] x INPUT-BIG 3 i + + c! loop
   s" </r>" drop INPUT-BIG 8195 + 4 BYTE-COPY
   INPUT-BIG 8199 3 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   0 INPUT-USED ! 3 INPUT-OFF !
   begin NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      RAW {: start:off size:len :}
      start OFF>N INPUT-OFF @ T=
      TEXT-BUF TEXT-CAP TEXT {: count:n :}
      size LEN>N count T=
      count INPUT-USED +!
      size LEN>N INPUT-OFF +!
   repeat
   INPUT-USED @ 8192 T= INPUT-OFF @ 8195 T=
   RAW {: end-off:off end-len:len :}
   end-off OFF>N 8195 T= end-len LEN>N 4 T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-LARGE-SCOPE ( -- )
   s" <a><b xmlns:p='" {: prefix size:n :}
   prefix INPUT-BIG size BYTE-COPY
   1000 0 ?do [char] x INPUT-BIG size i + + c! loop
   s" '><p:c/></b><d/></a>" {: suffix suffix-size:n :}
   suffix INPUT-BIG size 1000 + + suffix-size BYTE-COPY
   INPUT-BIG size 1000 + suffix-size +
      1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   XML-KIND:START NEXT=
   XML-KIND:START NEXT= s" p:c" NAME=
   TEXT-BUF TEXT-CAP URI 1000 T=
   TEXT-BUF c@ [char] x T=
   TEXT-BUF 999 + c@ [char] x T=
   XML-KIND:END NEXT= XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" d" NAME=
   XML-KIND:END NEXT= XML-KIND:END NEXT=
   XML-KIND:EOF NEXT= XML:CLOSE ;

\ An input reader keeps the live frames' and bindings' strings in its scope
\ buffer while it drops consumed bytes, and one-byte fills grow the lexical
\ buffer within a tag. Shadowed prefixes must resolve and restore across
\ both, after ordinary and self-closing children, and the duplicate checks must
\ find a moved binding's URI equal to a fresh one spelled another way.
: INPUT-NAMESPACE-SCOPES ( -- )
   s" <a xmlns:p='urn:outer' xmlns='urn:d'><b xmlns:p='urn:inner'><p:c p:x='1'/><e xmlns:p='urn:self' p:y='2'/><p:f/></b><p:g p:z='3'/><h a0='0' a1='1' a2='2' a3='3' a4='4' a5='5' a6='6' a7='7' a8='8' a9='9' p:a0='x' xmlns:q='urn:outer' q:a1='y'/><i p:a0='1' a0='2'/></a>"
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT= s" urn:d" URI= s" p" s" urn:outer" PREFIX=
   XML-KIND:START NEXT= s" b" NAME= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" urn:inner" URI=
   ATTR-NEXT TTRUE s" urn:inner" ATTR-URI=
   XML-KIND:END NEXT= s" p:c" NAME= s" urn:inner" URI=
   XML-KIND:START NEXT= s" e" NAME= s" urn:d" URI= s" p" s" urn:self" PREFIX=
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE s" urn:self" ATTR-URI=
   XML-KIND:END NEXT= s" e" NAME= s" p" s" urn:self" PREFIX=
   XML-KIND:START NEXT= s" p:f" NAME= s" urn:inner" URI=
   s" p" s" urn:inner" PREFIX=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" b" NAME= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" p:g" NAME= s" urn:outer" URI=
   s" p" s" urn:outer" PREFIX=
   ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" h" NAME= s" q" s" urn:outer" PREFIX=
   s" " s" urn:d" PREFIX=
   12 0 ?do ATTR-NEXT drop loop
   ATTR-NEXT TTRUE s" q:a1" ATTR-NAME= s" urn:outer" ATTR-URI=
   ATTR-NEXT TFALSE
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" i" NAME= s" q" PREFIX-MISSING
   ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   ATTR-NEXT TTRUE s" a0" ATTR-NAME= s" " ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" a" NAME= s" urn:d" URI= s" p" s" urn:outer" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE
   s" <a xmlns:p='urn:u'><b xmlns:r='urn:v'><c xmlns:q='urn:&#117;' r:x='0' p:x='1' q:x='2'/></b></a>"
      E-NAMESPACE INPUT-BAD
   s" <a xmlns:p='urn:u' xmlns:q='urn:u'><b><c p:x='1' q:x='2'/></b></a>"
      E-NAMESPACE INPUT-BAD ;

: DEEP+ ( ptr u8 n -- ) BUF:N>BLEN DEEP-DOC BUF:APPEND-SPAN ;

\ Level i of the chain is p:aD, D the digit i mod 10.
: LEVEL-NAME ( n -- ptr u8 n )
   10 mod [char] 0 + {: digit:n :}
   s" p:a" drop DEEP-NAME 3 BYTE-COPY
   digit DEEP-NAME 3 + c!
   DEEP-NAME 4 ;

: DEEP-BUILD ( -- ptr u8 n )
   DEEP-DOC 16 BUF:N>BLEN BUF:INIT
   s" <r xmlns:p='urn:p'>" DEEP+
   DEEP-LEVELS 0 ?do s" <" DEEP+ i LEVEL-NAME DEEP+ s" >" DEEP+ loop
   s" <p:z/>" DEEP+
   0 DEEP-LEVELS 1- ?do s" </" DEEP+ i LEVEL-NAME DEEP+ s" >" DEEP+ -1 +loop
   s" </r>" DEEP+
   DEEP-DOC BUF:SPAN$ BUF:BLEN>N ;

: DEEP-EVENT ( XML:reader XML:kind n -- XML:reader )
   {: expected:XML:kind level:n :}
   NEXT TOKEN>N expected TOKEN>N = 0= if 1 DEEP-BAD +! exit then
   NAME$ level LEVEL-NAME STR= 0= if 1 DEEP-BAD +! then ;

\ A chain 100000 elements deep streams through one-byte fills. Each level's name,
\ which its end tag must match, and the root's binding live in scope storage
\ while consumed bytes are dropped; reading costs time linear in the bytes,
\ whatever the depth, or the row overruns its CPU budget.
: INPUT-DEEP-CHAIN ( -- )
   DEEP-BUILD 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-OPEN
   0 DEEP-BAD !
   XML-KIND:START NEXT= s" r" NAME=
   DEEP-LEVELS 0 ?do XML-KIND:START i DEEP-EVENT loop
   DEEP-BAD @ 0 T=
   XML-KIND:START NEXT= s" p:z" NAME= s" urn:p" URI=
   XML:DEPTH DEEP-LEVELS 2 + T= s" p" s" urn:p" PREFIX=
   XML-KIND:END NEXT= s" p:z" NAME=
   0 DEEP-LEVELS 1- ?do XML-KIND:END i DEEP-EVENT -1 +loop
   DEEP-BAD @ 0 T=
   XML-KIND:END NEXT= s" r" NAME= s" p" s" urn:p" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE
   DEEP-DOC BUF:DISPOSE ;

\ The reader's element, attribute and binding tables start small and grow: a
\ 5000-deep tree, a tag of 2000 attributes and 2000 live bindings read whole.
5000 constant TALL-LEVELS
2000 constant WIDE-ATTRS
1000 constant NS-LEVELS
variable SEEN

: DOC-START ( -- ) DEEP-DOC 16 BUF:N>BLEN BUF:INIT ;
: DOC$ ( -- ptr u8 n ) DEEP-DOC BUF:SPAN$ BUF:BLEN>N ;

: DEEP-N+ ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod $30 + DEEP-NAME c!
   DEEP-NAME 1 DEEP+ ;

\ Read count events, counting each that is not expected in DEEP-BAD.
: EVENTS ( XML:reader XML:kind n -- XML:reader )
   {: expected:XML:kind count:n :}
   count 0 ?do
      NEXT TOKEN>N expected TOKEN>N <> if 1 DEEP-BAD +! then
   loop ;

: TALL-TREE ( -- )
   DOC-START
   TALL-LEVELS 0 ?do s" <a>" DEEP+ loop
   TALL-LEVELS 0 ?do s" </a>" DEEP+ loop
   0 DEEP-BAD !
   DOC$ OPEN
   XML-KIND:START TALL-LEVELS EVENTS
   XML:DEPTH TALL-LEVELS T=
   XML-KIND:END TALL-LEVELS EVENTS
   DEEP-BAD @ 0 T=
   XML-KIND:EOF NEXT= XML:CLOSE
   DEEP-DOC BUF:DISPOSE ;

: MANY-ATTRIBUTES ( -- )
   DOC-START s" <a" DEEP+
   WIDE-ATTRS 0 ?do
      s"  a" DEEP+ i DEEP-N+ s" ='" DEEP+ i DEEP-N+ s" '" DEEP+
   loop
   s" />" DEEP+
   DOC$ OPEN XML-KIND:START NEXT=
   0 SEEN !
   begin ATTR-NEXT while 1 SEEN +! repeat
   SEEN @ WIDE-ATTRS T=
   ATTR-RESET WIDE-ATTRS 0 ?do ATTR-NEXT drop loop
   s" a1999" ATTR-NAME= s" 1999" ATTR-TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   DEEP-DOC BUF:DISPOSE ;

\ Level i rebinds p to urn:i and binds its own qi to urn:qi. Rebuilding the
\ prefix buckets as the binding table grows keeps every binding reachable, and
\ each pop restores the binding it shadowed.
: MANY-BINDINGS ( -- )
   DOC-START
   NS-LEVELS 0 ?do
      s" <p:e xmlns:p='urn:" DEEP+ i DEEP-N+
      s" ' xmlns:q" DEEP+ i DEEP-N+ s" ='urn:q" DEEP+ i DEEP-N+ s" '>" DEEP+
   loop
   NS-LEVELS 0 ?do s" </p:e>" DEEP+ loop
   0 DEEP-BAD !
   DOC$ OPEN
   XML-KIND:START NS-LEVELS EVENTS
   s" urn:999" URI= s" p" s" urn:999" PREFIX=
   s" q0" s" urn:q0" PREFIX= s" q999" s" urn:q999" PREFIX=
   XML-KIND:END NS-LEVELS 1- EVENTS
   XML-KIND:END NEXT= s" urn:0" URI= s" p" s" urn:0" PREFIX=
   s" q0" s" urn:q0" PREFIX= s" q1" PREFIX-MISSING
   DEEP-BAD @ 0 T=
   XML-KIND:EOF NEXT= XML:CLOSE
   DEEP-DOC BUF:DISPOSE ;

: INPUT-FAILURES ( -- )
   s" <a><b></a>" E-MALFORMED INPUT-BAD
   s" <a/>x" E-MALFORMED INPUT-BAD
   s" <a/><b/>" E-MALFORMED INPUT-BAD
   s" " E-MALFORMED INPUT-BAD
   s" <a>&amp" E-TRUNCATED INPUT-BAD
   s" <!DOCTYPE a><a/>" E-DTD INPUT-BAD
   s\" \xFF\xFE<a/>" E-ENCODING INPUT-BAD
   s" <?xml version='1.0' encoding='UTF-16'?><a/>" E-ENCODING INPUT-BAD
   s" <a>&#12<later" 1 8 INPUT-SET
   INPUT-FAILURE E-ENTITY T=
   s" <a>&#1114112;tail" 1 12 INPUT-SET
   INPUT-FAILURE E-ENTITY T=
   s" <a>&#x110000;tail" 1 12 INPUT-SET
   INPUT-FAILURE E-ENTITY T=
   s" <1x/>" 1 2 INPUT-SET
   INPUT-FAILURE E-MALFORMED T=
   s\" <\xFFx/>" 1 2 INPUT-SET
   INPUT-FAILURE E-UTF8 T=
   s\" <\xE0\x80x/>" 1 3 INPUT-SET
   INPUT-FAILURE E-UTF8 T=
   s\" <a\xFFx/>" 1 3 INPUT-SET
   INPUT-FAILURE E-UTF8 T=
   s\" <a>\zx</a>" 1 4 INPUT-SET
   INPUT-FAILURE E-SCALAR T=
   s\" <a>\r\n</a>" 1 4 INPUT-SET
   INPUT-FAILURE -9299 T=
   s" <a>&#00000000000065;</a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-OPEN XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   s" A" TEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   INPUT-OVER-COUNT-RC E-RANGE T=
   s" <a/>" 1 2 INPUT-SET
   INPUT-FAILURE -9299 T= ;

T-RESET
EMPTY-ELEMENT
NAMESPACE-SCOPES
NAMESPACE-ENTITIES
PREFIX-VALUES
PREFIX-ENTITY-AND-SCOPE
SCALARS-AND-OFFSETS
ATTRIBUTES-AND-SPANS
MARKUP-TEXT
UNICODE-NAMES
MALFORMED-INPUT
BAD-ENTITIES
BAD-NAMESPACES
SIBLING-ATTRIBUTES
NAMESPACE-SHADOWING
BAD-ENCODINGS
ASCII-WIRE
FAILURE-AND-STATE
NAMESPACE-FAILURE
NO-PARTIAL-DECODE
INIT-RECORD-ALIAS
TEXT-RECORD-ALIAS
URI-CAPACITY-RECOVERY
PREFIX-RECOVERY
PREFIX-READERS
ABANDONED-RECORD
ATTRIBUTE-CAPACITY-RECOVERY
WIDE-ELEMENT
ESCAPING
CHECKED-OWNERSHIP
INPUT-BOUNDARIES
INPUT-MARKUP
INPUT-NEWLINE
INPUT-ALIAS-CHECK
INPUT-LARGE-TEXT
INPUT-LARGE-SCOPE
INPUT-NAMESPACE-SCOPES
INPUT-DEEP-CHAIN
INPUT-FAILURES
TALL-TREE
MANY-ATTRIBUTES
MANY-BINDINGS
T-REPORT
;using
;package
