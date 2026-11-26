require src/lib/test/assert.f
require src/lib/xml/names.f
require src/lib/xml/read.f
require src/lib/xml/scalar.f
require src/lib/xml/state.f
require src/lib/xml/write.f
require src/lib/xml.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/reason.f
require src/lib/test/counting-allocator.f

package XML-TEST

$400 constant TEXT-CAP
create OTHER-BUF TEXT-CAP allot
PTR-VARIABLE INPUT-SOURCE
variable INPUT-LEN
variable INPUT-POS
variable INPUT-LIMIT
variable INPUT-FAIL
variable INPUT-OFF
variable INPUT-USED
create INPUT-BIG 8200 allot
TYPED-VARIABLE DEEP-DOC ^VEC:vector<u8>
TYPED-VARIABLE DECODED ^VEC:vector<u8>
TYPED-VARIABLE ESCAPED-OUT ^VEC:vector<u8>
100000 constant DEEP-LEVELS
4 BUFFER: DEEP-NAME
variable DEEP-BAD
TYPED-VARIABLE RD XML:cursor
TYPED-VARIABLE RD2 XML:cursor

CAST: TOKEN>N ( XML:kind -- n )

: OPEN ( str -- XML:reader )
   {: source:str :}
   RD HEAP:ALLOCATOR source XML:INIT ;

: NEXT= ( XML:reader XML:kind -- XML:reader )
   TOKEN>N {: expected:n :}
   XML:NEXT TOKEN>N expected T= ;

: RAW= ( XML:reader n n -- XML:reader )
   {: start:n size:n :}
   XML:RAW LEN>N size T= OFF>N start T= ;

: NAME= ( XML:reader str -- XML:reader )
   {: expected:str :}
   XML:NAME$ expected T$= ;

: LOCAL= ( XML:reader str -- XML:reader )
   {: expected:str :}
   XML:LOCAL$ expected T$= ;

: DECODED-NEW ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DECODED ! ;
: DECODED-CLEAR ( -- ) DECODED @ 0 VEC:RESIZE ;
: CLEARED ( -- ^VEC:vector<u8> ) DECODED-CLEAR DECODED @ ;
: DECODED$ ( -- str ) DECODED @ VEC:BYTES ;

: URI= ( XML:reader str -- XML:reader )
   {: expected:str :}
   CLEARED XML:URI DECODED$ expected T$= ;

: PREFIX= ( XML:reader str str -- XML:reader )
   {: prefix:str expected:str :}
   prefix T-LABEL
   prefix CLEARED XML:PREFIX-URI TTRUE
   DECODED$ expected T$= ;

: PREFIX-MISSING ( XML:reader str -- XML:reader )
   {: prefix:str :}
   s" Z" CLEARED VEC:APPEND
   prefix DECODED @ XML:PREFIX-URI TFALSE
   DECODED$ s" Z" T$= ;

: PREFIX-READ ( XML:reader -- XML:reader )
   s" p" CLEARED XML:PREFIX-URI drop ;

: TEXT= ( XML:reader str -- XML:reader )
   {: expected:str :}
   CLEARED XML:TEXT DECODED$ expected T$= ;

: ATTR-NAME= ( XML:reader str -- XML:reader )
   {: expected:str :}
   XML:ATTR-NAME$ expected T$= ;

: ATTR-URI= ( XML:reader str -- XML:reader )
   {: expected:str :}
   CLEARED XML:ATTR-URI DECODED$ expected T$= ;

: ATTR-TEXT= ( XML:reader str -- XML:reader )
   {: expected:str :}
   CLEARED XML:ATTR-TEXT DECODED$ expected T$= ;

: DRAIN ( XML:reader -- XML:reader )
   begin XML:NEXT TOKEN>N XML-KIND:EOF TOKEN>N <> while repeat ;

: INPUT-SET ( str n n -- )
   {: source:str limit:n fail-at:n :}
   source STR:DATA INPUT-SOURCE ! source STR:LEN INPUT-LEN !
   limit INPUT-LIMIT ! fail-at INPUT-FAIL !
   0 INPUT-POS ! ;

: INPUT-FILL ( ^n SPAN:span<u8> -- n )
   {: pos:^ output :}
   pos @ INPUT-FAIL @ >= if -9299 throw then
   INPUT-LEN @ pos @ -
   output SPAN:LEN min INPUT-LIMIT @ min {: count:n :}
   count 0= if 0 exit then
   INPUT-SOURCE @ pos @ + count >str output SPAN:COPY
   count pos +! count ;

: INPUT-OPEN ( -- XML:reader )
   RD HEAP:ALLOCATOR INPUT-POS ['] INPUT-FILL XML:INIT-INPUT ;

: INPUT-FAILURE ( -- n )
   INPUT-OPEN ['] DRAIN catch swap XML:CLOSE ;

\ Answers one byte past the span it was lent.
: INPUT-OVER-COUNT ( ^n SPAN:span<u8> -- n )
   {: pos:^ output :}
   output SPAN:LEN 1 + ;

: INPUT-OVER-COUNT-RC ( -- n )
   RD HEAP:ALLOCATOR INPUT-POS ['] INPUT-OVER-COUNT XML:INIT-INPUT
   ['] DRAIN catch swap XML:CLOSE ;

: INPUT-BAD ( str n -- )
   {: source:str expected:n :}
   source 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE expected T= ;

\ The reader opens inside the caught body: `catch` restores each stack's
\ depth, not its contents, so a reader opened before it comes back stale.
: BAD ( str n -- )
   {: expected:n :}
   [: dup OPEN ['] DRAIN catch swap XML:CLOSE throw ;] catch expected T=
   drop ;

: BAD-INIT ( str n -- )
   {: expected:n :}
   [: dup OPEN XML:CLOSE ;] catch expected T=
   drop ;

: EMPTY-ELEMENT ( -- )
   s" <a/>" OPEN
   XML-KIND:START NEXT= 0 4 RAW= s" a" NAME=
   XML:DEPTH 1 T=
   XML:ATTR-NEXT TFALSE
   XML-KIND:END NEXT= 4 0 RAW= s" a" NAME=
   XML:DEPTH 1 T=
   XML-KIND:EOF NEXT= 4 0 RAW= XML:DEPTH 0 T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: NAMESPACE-SCOPES ( -- )
   s\" <a xmlns='urn:outer' xmlns:p='urn:p' plain='x' p:id='1'><p:b xmlns:p='urn:inner'><c xmlns=''/></p:b></a>" OPEN
   XML-KIND:START NEXT= s" urn:outer" URI=
   XML:ATTR-NEXT TTRUE s" xmlns" ATTR-NAME=
   s" http://www.w3.org/2000/xmlns/" ATTR-URI=
   XML:ATTR-NEXT TTRUE s" xmlns:p" ATTR-NAME=
   XML:ATTR-NEXT TTRUE s" plain" ATTR-NAME= s" " ATTR-URI=
   XML:ATTR-NEXT TTRUE s" p:id" ATTR-NAME= s" urn:p" ATTR-URI=
   XML:ATTR-LOCAL$ s" id" T$=
   XML:ATTR-NEXT TFALSE
   XML:ATTR-RESET XML:ATTR-NEXT TTRUE s" xmlns" ATTR-NAME=
   XML-KIND:START NEXT= s" p:b" NAME= s" b" LOCAL= s" urn:inner" URI=
   XML-KIND:START NEXT= s" " URI=
   XML-KIND:END NEXT= s" " URI=
   XML-KIND:END NEXT= s" urn:inner" URI=
   XML-KIND:END NEXT= s" urn:outer" URI=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: NAMESPACE-ENTITIES ( -- )
   s\" <p:a p:x='1' xmlns:p='urn:&#x61;' xml:lang='en'/>" OPEN
   XML-KIND:START NEXT= s" urn:a" URI=
   XML:ATTR-NEXT TTRUE s" urn:a" ATTR-URI=
   XML:ATTR-NEXT TTRUE
   XML:ATTR-NEXT TTRUE s" http://www.w3.org/XML/1998/namespace" ATTR-URI=
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
   XML:ATTR-NEXT TTRUE s" Requires" ATTR-NAME=
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
   ['] PREFIX-READ catch XML:E-STATE T=
   XML-KIND:START NEXT=
   s" p" s" urn:a z" PREFIX=
   XML-KIND:COMMENT NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:PI NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:START NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:END NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:END NEXT= s" p" s" urn:a z" PREFIX=
   XML-KIND:COMMENT NEXT= ['] PREFIX-READ catch XML:E-STATE T=
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
   XML:ATTR-NEXT TTRUE
   XML:ATTR-RAW LEN>N 45 T= OFF>N 3 T=
   s\" x y &\t\r\q'<>" ATTR-TEXT=
   XML:ATTR-VALUE$ s\" x\r\ny\t&amp;&#9;&#13;&quot;&apos;&lt;&gt;" T$=
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
   s" " XML:E-MALFORMED BAD
   s" <a>" XML:E-TRUNCATED BAD
   s" <a" XML:E-TRUNCATED BAD
   s" <a></b>" XML:E-MALFORMED BAD
   s" <a/><b/>" XML:E-MALFORMED BAD
   s" x<a/>" XML:E-MALFORMED BAD
   s" <a/>x" XML:E-MALFORMED BAD
   s" <a>]]></a>" XML:E-MALFORMED BAD
   s" <!--a--b--><a/>" XML:E-MALFORMED BAD
   s" <!--a" XML:E-TRUNCATED BAD
   s" <![CDATA[x]]><a/>" XML:E-MALFORMED BAD
   s" <a><![CDATA[x" XML:E-TRUNCATED BAD
   s" <?go" XML:E-TRUNCATED BAD
   s" <?go?><a/>" OPEN DRAIN XML:CLOSE
   s" <a q='x'v='y'/>" XML:E-MALFORMED BAD
   s" <a q='<x'/>" XML:E-MALFORMED BAD
   s" <a q='x/>" XML:E-TRUNCATED BAD
   s" <a></a x='1'>" XML:E-MALFORMED BAD
   s" <!DOCTYPE a [<!ENTITY x SYSTEM 'file:///no'>]><a/>" XML:E-DTD BAD ;

: BAD-ENTITIES ( -- )
   s" <a>&unknown;</a>" XML:E-ENTITY BAD
   s" <a>&amp</a>" XML:E-ENTITY BAD
   s" <a>&#;</a>" XML:E-ENTITY BAD
   s" <a>&#x;</a>" XML:E-ENTITY BAD
   s" <a>&#x110000;</a>" XML:E-ENTITY BAD
   s" <a>&#99999999999999999999;</a>" XML:E-ENTITY BAD
   s" <a>&#xD800;</a>" XML:E-SCALAR BAD
   s" <a>&#0;</a>" XML:E-SCALAR BAD
   s" <a>&#12;</a>" XML:E-SCALAR BAD
   s" <a>&#X20;</a>" XML:E-ENTITY BAD ;

: BAD-NAMESPACES ( -- )
   s" <p:a/>" XML:E-NAMESPACE BAD
   s" <a p:q='x'/>" XML:E-NAMESPACE BAD
   s" <:a/>" XML:E-NAMESPACE BAD
   s" <p:/>" XML:E-NAMESPACE BAD
   s" <p:1/>" XML:E-NAMESPACE BAD
   s" <p:a:b/>" XML:E-NAMESPACE BAD
   s" <a q='1' q='2'/>" XML:E-NAMESPACE BAD
   s" <a xmlns:p='u' xmlns:p='v'/>" XML:E-NAMESPACE BAD
   s" <a xmlns:p='u0' xmlns:q='u&#48;' p:x='1' q:x='2'/>" XML:E-NAMESPACE BAD
   s" <a xmlns:p=''/>" XML:E-NAMESPACE BAD
   s" <a xmlns:xml='other'/>" XML:E-NAMESPACE BAD
   s" <a xmlns:xmlns='u'/>" XML:E-NAMESPACE BAD
   s" <a xmlns='http://www.w3.org/XML/1998/namespace'/>" XML:E-NAMESPACE BAD
   s" <a xmlns:p='http://www.w3.org/2000/xmlns/'/>" XML:E-NAMESPACE BAD ;

\ Each start tag reuses the attribute table: a sibling with fewer or more
\ attributes than the one before still finds its own duplicates, and only them.
: SIBLING-ATTRIBUTES ( -- )
   s" <r><a x='1' y='2' z='3'/><b z='1' w='2'/></r>" OPEN DRAIN XML:CLOSE
   s" <r><a x='1' y='2' z='3'/><b y='1' y='2'/></r>" XML:E-NAMESPACE BAD
   s" <r><a x='1'/><b y='1' z='2' y='3'/></r>" XML:E-NAMESPACE BAD
   s" <r xmlns:p='u' xmlns:q='u'><a p:x='1' q:y='2' z='3'/><b z='1' p:x='2' q:x='3'/></r>"
      XML:E-NAMESPACE BAD ;

\ A child's rebinding is seen by its descendants and ends with it, whether it
\ closes with an end tag or self-closes.
: NAMESPACE-SHADOWING ( -- )
   s" <a xmlns:p='urn:outer'><b xmlns:p='urn:inner'><p:c p:x='1'/></b><p:m/><d xmlns:p='urn:self' p:x='2'/><p:e p:x='3'/></a>" OPEN
   XML-KIND:START NEXT= s" p" s" urn:outer" PREFIX=
   XML-KIND:START NEXT= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" urn:inner" URI= XML:ATTR-NEXT TTRUE s" urn:inner" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" b" NAME= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" urn:outer" URI= s" p" s" urn:outer" PREFIX=
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" p" s" urn:self" PREFIX=
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE s" urn:self" ATTR-URI=
   XML-KIND:END NEXT= s" d" NAME=
   XML-KIND:START NEXT= s" urn:outer" URI= s" p" s" urn:outer" PREFIX=
   XML:ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" a" NAME= s" p" s" urn:outer" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: BAD-ENCODINGS ( -- )
   s\" \xFF\xFE<\z" XML:E-ENCODING BAD-INIT
   s\" \xFE\xFF\z<" XML:E-ENCODING BAD-INIT
   s\" <\za\z/\z>\z" XML:E-ENCODING BAD-INIT
   s\" \z<\za\z/\z>" XML:E-ENCODING BAD-INIT
   s" <?xml version='1.0' encoding='UTF-16'?><a/>" XML:E-ENCODING BAD
   s" <?xml version='1.1'?><a/>" XML:E-ENCODING BAD
   s" <?XML version='1.0'?><a/>" XML:E-MALFORMED BAD
   s"  <?xml version='1.0'?><a/>" XML:E-MALFORMED BAD
   s" <?xml encoding='UTF-8' version='1.0'?><a/>" XML:E-MALFORMED BAD
   s" <?xml version='1.0' standalone='maybe'?><a/>" XML:E-MALFORMED BAD
   s\" <a>\xC0\x80</a>" XML:E-UTF8 BAD
   s\" <a>\xED\xA0\x80</a>" XML:E-UTF8 BAD
   s\" <a>\xF4\x90\x80\x80</a>" XML:E-UTF8 BAD
   s\" <a>\z</a>" XML:E-SCALAR BAD ;

: ASCII-WIRE ( -- )
   s\" <?xml version=\q1.0\q encoding='Us-AsCiI'?><a q='&#x4E2D;'>&#x4E2D;</a>"
   OPEN XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE s" 中" ATTR-TEXT=
   XML-KIND:TEXT NEXT= s" 中" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><!--é--></a>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><![CDATA[é]]></a>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/><?p é?>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>é</a>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>é"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a x='é'/>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><é/>"
      XML:E-ENCODING BAD
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
      XML:E-ENCODING BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>é</a>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><!--é--></a>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><![CDATA[é]]></a>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/><?p é?>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><é/>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a q='é'/>"
      XML:E-ENCODING INPUT-BAD
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>é"
      XML:E-ENCODING INPUT-BAD
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
      XML:E-ENCODING INPUT-BAD
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
\ `using` import resolves.
: NEXT-ONE ( XML:reader -- XML:reader ) XML:NEXT drop ;
: ATTR-NEXT-ONE ( XML:reader -- XML:reader ) XML:ATTR-NEXT drop ;
: NAME-READ ( XML:reader -- XML:reader ) XML:NAME$ drop ;
: ATTR-NAME-READ ( XML:reader -- XML:reader ) XML:ATTR-NAME$ drop ;
: ATTR-RAW-READ ( XML:reader -- XML:reader ) XML:ATTR-RAW 2drop ;
: ATTR-VALUE-READ ( XML:reader -- XML:reader ) XML:ATTR-VALUE 2drop ;
: ATTR-VALUE$-READ ( XML:reader -- XML:reader ) XML:ATTR-VALUE$ drop ;
: CONTENT-READ ( XML:reader -- XML:reader ) XML:CONTENT 2drop ;
: TEXT-FULL ( XML:reader -- XML:reader ) CLEARED XML:TEXT ;

: FAILURE-AND-STATE ( -- )
   s" <a><b></a>" OPEN
   ['] DRAIN catch XML:E-MALFORMED T=
   ['] NEXT-ONE catch XML:E-STATE T=
   XML:CLOSE
   s" <a q='x'/>" OPEN
   ['] ATTR-NEXT-ONE catch XML:E-STATE T=
   ['] XML:ATTR-RESET catch XML:E-STATE T=
   ['] NAME-READ catch XML:E-STATE T=
   ['] ATTR-RAW-READ catch XML:E-STATE T=
   ['] ATTR-VALUE-READ catch XML:E-STATE T=
   ['] ATTR-VALUE$-READ catch XML:E-STATE T=
   XML-KIND:START NEXT=
   ['] CONTENT-READ catch XML:E-STATE T=
   ['] TEXT-FULL catch XML:E-STATE T=
   s" a" NAME=
   ['] ATTR-NAME-READ catch XML:E-STATE T=
   XML:ATTR-NEXT TTRUE s" q" ATTR-NAME=
   XML:ATTR-NEXT TFALSE
   ['] ATTR-NAME-READ catch XML:E-STATE T=
   XML:CLOSE ;

\ A namespace error once bindings exist fails the reader. A new reader starts
\ with none of them.
: NAMESPACE-FAILURE ( -- )
   s" <a xmlns:p='urn:stale'><b xmlns:q='urn:q' q:x='1' x='2' x='3'/></a>" OPEN
   XML-KIND:START NEXT=
   ['] NEXT-ONE catch XML:E-NAMESPACE T=
   ['] PREFIX-READ catch XML:E-STATE T=
   ['] NAME-READ catch XML:E-STATE T=
   ['] NEXT-ONE catch XML:E-STATE T=
   XML:CLOSE
   s" <c xmlns:r='urn:r'><d/></c>" OPEN
   XML-KIND:START NEXT=
   s" p" PREFIX-MISSING s" q" PREFIX-MISSING s" r" s" urn:r" PREFIX=
   XML-KIND:START NEXT= s" p" PREFIX-MISSING s" r" s" urn:r" PREFIX=
   XML-KIND:END NEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

\ A second TEXT appends after the first's bytes.
: TEXT-APPENDS ( -- )
   s" <a>long&amp;text</a>" OPEN
   XML:NEXT drop XML:NEXT drop
   CLEARED XML:TEXT DECODED @ XML:TEXT
   DECODED$ s" long&textlong&text" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: PREFIX-RECOVERY ( -- )
   s" <a xmlns:p='urn:&#97;'/>" OPEN
   ['] PREFIX-READ catch XML:E-STATE T=
   XML-KIND:START NEXT=
   s" " s" " PREFIX=
   s" p" s" urn:a" PREFIX=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   ['] PREFIX-READ catch XML:E-STATE T=
   XML:CLOSE
   s" <a><b></a>" OPEN
   ['] DRAIN catch XML:E-MALFORMED T=
   ['] PREFIX-READ catch XML:E-STATE T=
   XML:CLOSE ;

: PREFIX-READERS ( -- )
   RD HEAP:ALLOCATOR s" <a xmlns:p='urn:first'/>" XML:INIT
   RD2 HEAP:ALLOCATOR s" <b xmlns:p='urn:second'/>" XML:INIT
   XML-KIND:START NEXT= s" p" s" urn:second" PREFIX=
   swap XML-KIND:START NEXT= s" p" s" urn:first" PREFIX=
   swap s" p" s" urn:second" PREFIX=
   XML:CLOSE XML:CLOSE ;

77 constant CALLER-CODE

: ALWAYS ( -- bool ) 0 0= ;

: CALLER-FAIL ( XML:reader -- XML:reader )
   ALWAYS if CALLER-CODE throw then ;

\ The caller's own throw abandons a live reader, so record RD keeps its tables
\ and, for an input reader, its stream.
: ABANDON ( -- )
   RD ALLOC-TEST:COUNTING s" <a><b/></a>" XML:INIT
   XML-KIND:START NEXT= XML-KIND:START NEXT=
   CALLER-FAIL
   XML:CLOSE ;

: ABANDON-INPUT ( -- )
   s" <a><b/></a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET
   RD ALLOC-TEST:COUNTING INPUT-POS ['] INPUT-FILL XML:INIT-INPUT
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
   RD ALLOC-TEST:COUNTING s" <c/>" XML:INIT
   XML-KIND:START NEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE ;

\ The body opens its readers on the counting allocator.
: RELEASES ( [ -- ] -- )
   0 ALLOC-TEST:TAKEN !  0 ALLOC-TEST:FREED !
   execute
   ALLOC-TEST:TAKEN @ 0 > TTRUE
   ALLOC-TEST:TAKEN @ ALLOC-TEST:FREED @ T= ;

: ABANDONED-RECORD ( -- )
   s" DISPOSE releases what a caller's throw abandoned" T-LABEL
   [: ABANDON-DISPOSE ;] RELEASES
   s" a record holding a leftover reads again and gives back every block" T-LABEL
   [: ABANDON-REINIT ;] RELEASES ;

\ A failed build gives back what it took: a rejected INIT, and an INIT-INPUT
\ rejected at each of its requests in turn until one build succeeds.
: FAILED-BUILDS ( -- )
   s" a rejected INIT keeps nothing" T-LABEL
   [: [: RD ALLOC-TEST:COUNTING s\" \xFF\xFE<\z" XML:INIT XML:CLOSE ;] catch
      XML:E-ENCODING T= ;] RELEASES
   s" a rejected INIT-INPUT request keeps nothing" T-LABEL
   1 begin
      {: k:n :}
      0 ALLOC-TEST:TAKEN !  0 ALLOC-TEST:FREED !  k ALLOC-TEST:REJECTED !
      s" <a/>" 1 $7FFFFFFFFFFFFFFF INPUT-SET
      [: RD ALLOC-TEST:COUNTING INPUT-POS ['] INPUT-FILL XML:INIT-INPUT XML:CLOSE ;]
      catch {: code:n :}
      ALLOC-TEST:TAKEN @ ALLOC-TEST:FREED @ T=
      code 0<>
   while
      code E-MEM-SIZE T=  k 1+
   repeat
   0 ALLOC-TEST:REJECTED ! ;

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

: WIDE+ ( str -- )
   {: source:str :}
   source STR:DATA WIDE-DOC @ WIDE-LEN @ + source STR:LEN >LEN BYTE-COPY-LEN
   source STR:LEN WIDE-LEN +! ;

: WIDE-N+ ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod $30 + WIDE-DOC @ WIDE-LEN @ + c!
   1 WIDE-LEN +! ;

: URI+ ( str -- )
   {: source:str :}
   source STR:DATA OTHER-BUF URI-LEN @ + source STR:LEN >LEN BYTE-COPY-LEN
   source STR:LEN URI-LEN +! ;

\ Emit URI index, its U+20061 scalars as UTF-8 or as character references.
: WIDE-URI ( n bool [ str -- ] -- )
   {: index:n refs:bool emit :}
   URI-SCALARS 0 ?do
      index i rshift 1 and 0= if
         s" a" emit execute
      else
         refs if s" &#x20061;" else s\" \xF0\xA0\x81\xA1" then emit execute
      then
   loop ;

: EXPECTED-URI$ ( n -- str )
   0 URI-LEN !
   false [: URI+ ;] WIDE-URI
   OTHER-BUF URI-LEN @ >str ;

: WIDE-BUILD ( str -- )
   {: tail:str :}
   0 WIDE-LEN !
   s" <r" WIDE+
   WIDE-PREFIXES 0 ?do
      s"  xmlns:p" WIDE+ i WIDE-N+ s" ='" WIDE+
      i false [: WIDE+ ;] WIDE-URI s" '" WIDE+
   loop
   WIDE-PREFIXES 0 ?do s"  p" WIDE+ i WIDE-N+ s" :a='1'" WIDE+ loop
   tail WIDE+
   s" />" WIDE+ ;

: WIDE-OPEN ( -- XML:reader ) RD HEAP:ALLOCATOR WIDE-DOC @ WIDE-LEN @ >str XML:INIT ;

: WIDE-BAD ( str -- )
   WIDE-BUILD
   [: WIDE-OPEN ['] DRAIN catch swap XML:CLOSE throw ;] catch XML:E-NAMESPACE T= ;

: WIDE-ELEMENT ( -- )
   WIDE-DOC-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop WIDE-DOC !
   s" " WIDE-BUILD
   WIDE-OPEN XML-KIND:START NEXT=
   s" p0" s" aaaaaaaaaaaaaaa" PREFIX=
   s" p32767" WIDE-LAST EXPECTED-URI$ PREFIX=
   WIDE-PREFIXES 2 * 0 ?do XML:ATTR-NEXT drop loop
   s" p32767:a" ATTR-NAME= WIDE-LAST EXPECTED-URI$ ATTR-URI=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   s"  p0:a='2'" WIDE-BAD
   0 URI-LEN !
   s"  xmlns:q='" URI+ WIDE-LAST true [: URI+ ;] WIDE-URI s" ' q:a='2'" URI+
   OTHER-BUF URI-LEN @ >str WIDE-BAD
   WIDE-DOC @ WIDE-DOC-CAP MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: ESCAPING ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   v ESCAPED-OUT !
   s\" A<&>\r\n\q'😀" v XML:ESCAPE-TEXT
   v VEC:BYTES s\" A&lt;&amp;&gt;&#13;\n\q'😀" T$=
   v 0 VEC:RESIZE
   s\" A<&>\r\n\t\q'😀" v XML:ESCAPE-ATTR
   v VEC:BYTES s" A&lt;&amp;&gt;&#13;&#10;&#9;&quot;&apos;😀" T$=
   v 0 VEC:RESIZE
   s" " v XML:ESCAPE-TEXT  v VEC:COUNT 0 T=
   [: s\" \z" ESCAPED-OUT @ XML:ESCAPE-TEXT ;] XML:E-SCALAR TTHROWSQ
   v VEC:COUNT 0 T=
   v VEC:DELETE ;

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
   XML:ATTR-NEXT TTRUE
   XML:ATTR-RAW {: attr-off:off attr-len:len :}
   attr-off OFF>N 44 T= attr-len LEN>N 9 T=
   XML:ATTR-VALUE {: value-off:off value-len:len :}
   value-off OFF>N 47 T= value-len LEN>N 5 T=
   s" &" ATTR-TEXT=
   54 INPUT-OFF ! DECODED-CLEAR
   begin XML:NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      XML:RAW {: start:off size:len :}
      start OFF>N INPUT-OFF @ T=
      DECODED @ XML:TEXT
      size LEN>N INPUT-OFF +!
   repeat
   INPUT-OFF @ 63 T=
   DECODED$ s" Aé&Z" T$=
   XML:RAW {: end-off:off end-len:len :}
   end-off OFF>N 63 T= end-len LEN>N 6 T=
   s" p" s" u" PREFIX=
   XML-KIND:START NEXT= 69 4 RAW=
   XML-KIND:END NEXT= 73 0 RAW=
   XML-KIND:END NEXT= 73 4 RAW=
   XML-KIND:TEXT NEXT= 77 1 RAW= s"  " TEXT=
   XML-KIND:EOF NEXT= 78 0 RAW=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-MARKUP ( -- )
   s" <a><!--long名😀--><![CDATA[é&]]><?go q?></a>"
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   XML-KIND:COMMENT NEXT= s" long名😀" TEXT=
   XML-KIND:CDATA NEXT= s" é&" TEXT=
   XML-KIND:PI NEXT= s" q" TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-NEWLINE ( -- )
   S\" <a>x\r\ny</a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   DECODED-CLEAR
   begin XML:NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      DECODED @ XML:TEXT
   repeat
   DECODED$ S\" x\ny" T$=
   XML:KIND TOKEN>N XML-KIND:END TOKEN>N T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

\ Fills of at most limit bytes; an unlimited one fills each span it is lent until input runs short.
: INPUT-LARGE-TEXT ( n -- ) {: limit:n :}
   s" <r>" STR:DATA INPUT-BIG 3 BYTE-COPY
   8192 0 ?do [char] x INPUT-BIG 3 i + + c! loop
   s" </r>" STR:DATA INPUT-BIG 8195 + 4 BYTE-COPY
   INPUT-BIG 8199 >str limit $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   0 INPUT-USED ! 3 INPUT-OFF !
   begin XML:NEXT TOKEN>N XML-KIND:TEXT TOKEN>N = while
      XML:RAW {: start:off size:len :}
      start OFF>N INPUT-OFF @ T=
      CLEARED XML:TEXT DECODED @ VEC:COUNT {: count:n :}
      size LEN>N count T=
      count INPUT-USED +!
      size LEN>N INPUT-OFF +!
   repeat
   INPUT-USED @ 8192 T= INPUT-OFF @ 8195 T=
   XML:RAW {: end-off:off end-len:len :}
   end-off OFF>N 8195 T= end-len LEN>N 4 T=
   XML-KIND:EOF NEXT= XML:CLOSE ;

: INPUT-LARGE-SCOPE ( -- )
   s" <a><b xmlns:p='" {: prefix:str :}
   prefix STR:DATA INPUT-BIG prefix STR:LEN BYTE-COPY
   1000 0 ?do [char] x INPUT-BIG prefix STR:LEN i + + c! loop
   s" '><p:c/></b><d/></a>" {: suffix:str :}
   suffix STR:DATA INPUT-BIG prefix STR:LEN 1000 + + suffix STR:LEN BYTE-COPY
   INPUT-BIG prefix STR:LEN 1000 + suffix STR:LEN + >str
      1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   XML-KIND:START NEXT=
   XML-KIND:START NEXT=
   XML-KIND:START NEXT= s" p:c" NAME=
   CLEARED XML:URI DECODED @ VEC:COUNT 1000 T=
   DECODED$ STR:DATA {: uri:^ :}
   uri c@ [char] x T=
   uri 999 + c@ [char] x T=
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
   XML:ATTR-NEXT TTRUE s" urn:inner" ATTR-URI=
   XML-KIND:END NEXT= s" p:c" NAME= s" urn:inner" URI=
   XML-KIND:START NEXT= s" e" NAME= s" urn:d" URI= s" p" s" urn:self" PREFIX=
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE s" urn:self" ATTR-URI=
   XML-KIND:END NEXT= s" e" NAME= s" p" s" urn:self" PREFIX=
   XML-KIND:START NEXT= s" p:f" NAME= s" urn:inner" URI=
   s" p" s" urn:inner" PREFIX=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" b" NAME= s" p" s" urn:inner" PREFIX=
   XML-KIND:START NEXT= s" p:g" NAME= s" urn:outer" URI=
   s" p" s" urn:outer" PREFIX=
   XML:ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" h" NAME= s" q" s" urn:outer" PREFIX=
   s" " s" urn:d" PREFIX=
   12 0 ?do XML:ATTR-NEXT drop loop
   XML:ATTR-NEXT TTRUE s" q:a1" ATTR-NAME= s" urn:outer" ATTR-URI=
   XML:ATTR-NEXT TFALSE
   XML-KIND:END NEXT=
   XML-KIND:START NEXT= s" i" NAME= s" q" PREFIX-MISSING
   XML:ATTR-NEXT TTRUE s" urn:outer" ATTR-URI=
   XML:ATTR-NEXT TTRUE s" a0" ATTR-NAME= s" " ATTR-URI=
   XML-KIND:END NEXT=
   XML-KIND:END NEXT= s" a" NAME= s" urn:d" URI= s" p" s" urn:outer" PREFIX=
   XML-KIND:EOF NEXT= XML:CLOSE
   s" <a xmlns:p='urn:u'><b xmlns:r='urn:v'><c xmlns:q='urn:&#117;' r:x='0' p:x='1' q:x='2'/></b></a>"
      XML:E-NAMESPACE INPUT-BAD
   s" <a xmlns:p='urn:u' xmlns:q='urn:u'><b><c p:x='1' q:x='2'/></b></a>"
      XML:E-NAMESPACE INPUT-BAD ;

: DOC-START ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DEEP-DOC ! ;
: DOC$ ( -- str ) DEEP-DOC @ VEC:BYTES ;
: DEEP+ ( str -- ) DEEP-DOC @ VEC:APPEND ;

\ Level i of the chain is p:aD, D the digit i mod 10.
: LEVEL-NAME ( n -- str )
   10 mod [char] 0 + {: digit:n :}
   s" p:a" STR:DATA DEEP-NAME 3 BYTE-COPY
   digit DEEP-NAME 3 + c!
   DEEP-NAME 4 >str ;

: DEEP-BUILD ( -- str )
   DOC-START
   s" <r xmlns:p='urn:p'>" DEEP+
   DEEP-LEVELS 0 ?do s" <" DEEP+ i LEVEL-NAME DEEP+ s" >" DEEP+ loop
   s" <p:z/>" DEEP+
   0 DEEP-LEVELS 1- ?do s" </" DEEP+ i LEVEL-NAME DEEP+ s" >" DEEP+ -1 +loop
   s" </r>" DEEP+
   DOC$ ;

: DEEP-EVENT ( XML:reader XML:kind n -- XML:reader )
   {: expected:XML:kind level:n :}
   XML:NEXT TOKEN>N expected TOKEN>N = 0= if 1 DEEP-BAD +! exit then
   XML:NAME$ level LEVEL-NAME STR:= 0= if 1 DEEP-BAD +! then ;

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
   DEEP-DOC @ VEC:DELETE ;

\ The reader's element, attribute and binding tables start small and grow: a
\ 5000-deep tree, a tag of 2000 attributes and 2000 live bindings read whole.
5000 constant TALL-LEVELS
2000 constant WIDE-ATTRS
1000 constant NS-LEVELS
variable SEEN


: DEEP-N+ ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod $30 + DEEP-NAME c!
   DEEP-NAME 1 >str DEEP+ ;

\ Read count events, counting each that is not expected in DEEP-BAD.
: EVENTS ( XML:reader XML:kind n -- XML:reader )
   {: expected:XML:kind count:n :}
   count 0 ?do
      XML:NEXT TOKEN>N expected TOKEN>N <> if 1 DEEP-BAD +! then
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
   DEEP-DOC @ VEC:DELETE ;

: MANY-ATTRIBUTES ( -- )
   DOC-START s" <a" DEEP+
   WIDE-ATTRS 0 ?do
      s"  a" DEEP+ i DEEP-N+ s" ='" DEEP+ i DEEP-N+ s" '" DEEP+
   loop
   s" />" DEEP+
   DOC$ OPEN XML-KIND:START NEXT=
   0 SEEN !
   begin XML:ATTR-NEXT while 1 SEEN +! repeat
   SEEN @ WIDE-ATTRS T=
   XML:ATTR-RESET WIDE-ATTRS 0 ?do XML:ATTR-NEXT drop loop
   s" a1999" ATTR-NAME= s" 1999" ATTR-TEXT=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   DEEP-DOC @ VEC:DELETE ;

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
   DEEP-DOC @ VEC:DELETE ;

: INPUT-FAILURES ( -- )
   s" <a><b></a>" XML:E-MALFORMED INPUT-BAD
   s" <a/>x" XML:E-MALFORMED INPUT-BAD
   s" <a/><b/>" XML:E-MALFORMED INPUT-BAD
   s" " XML:E-MALFORMED INPUT-BAD
   s" <a>&amp" XML:E-TRUNCATED INPUT-BAD
   s" <!DOCTYPE a><a/>" XML:E-DTD INPUT-BAD
   s\" \xFF\xFE<a/>" XML:E-ENCODING INPUT-BAD
   s" <?xml version='1.0' encoding='UTF-16'?><a/>" XML:E-ENCODING INPUT-BAD
   s" <a>&#12<later" 1 8 INPUT-SET
   INPUT-FAILURE XML:E-ENTITY T=
   s" <a>&#1114112;tail" 1 12 INPUT-SET
   INPUT-FAILURE XML:E-ENTITY T=
   s" <a>&#x110000;tail" 1 12 INPUT-SET
   INPUT-FAILURE XML:E-ENTITY T=
   s" <1x/>" 1 2 INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   s\" <\xFFx/>" 1 2 INPUT-SET
   INPUT-FAILURE XML:E-UTF8 T=
   s\" <\xE0\x80x/>" 1 3 INPUT-SET
   INPUT-FAILURE XML:E-UTF8 T=
   s\" <a\xFFx/>" 1 3 INPUT-SET
   INPUT-FAILURE XML:E-UTF8 T=
   s\" <a>\zx</a>" 1 4 INPUT-SET
   INPUT-FAILURE XML:E-SCALAR T=
   s\" <a>\r\n</a>" 1 4 INPUT-SET
   INPUT-FAILURE -9299 T=
   s" <a>&#00000000000065;</a>" 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-OPEN XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   s" A" TEXT= XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   INPUT-OVER-COUNT-RC XML:E-RANGE T=
   s" <a/>" 1 2 INPUT-SET
   INPUT-FAILURE -9299 T= ;

\ A rejection's text: the place, what was expected, the token found and the fix.
: SAID ( n str -- ) {: code:n ws:str :}
   code REASON:TEXT$ ws T$=
   REASON:RELEASE ;

: REJECTION-TEXTS ( -- )
   s" an end tag that closes another element names the open one" T-LABEL
   s" <a></b>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:6: expected `</a>`, found `b`; close elements in the reverse of the order they were opened" SAID
   s" a found run takes a UTF-8 character above ASCII whole" T-LABEL
   s\" <a></caf\xC3\xA9>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s\" 1:6: expected `</a>`, found `caf\xC3\xA9`; close elements in the reverse of the order they were opened" SAID
   s" a found run may start with a UTF-8 character above ASCII" T-LABEL
   s\" <a></\xC3\xA9crit>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s\" 1:6: expected `</a>`, found `\xC3\xA9crit`; close elements in the reverse of the order they were opened" SAID
   s" a found run cut inside a character ends before it" T-LABEL
   s\" <a></xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\xC3\xA9z>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:6: expected `</a>`, found `xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...`; close elements in the reverse of the order they were opened" SAID
   s" a found run writes a control or format character as U+XXXX" T-LABEL
   s\" <a></c\xE2\x80\xAEd\xC2\x85e>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:6: expected `</a>`, found `cU+202EdU+0085e`; close elements in the reverse of the order they were opened" SAID
   s" an expected end tag writes a format character in the open name as U+XXXX" T-LABEL
   s\" <a\xD8\x9C></b>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:8: expected `</aU+061C>`, found `b`; close elements in the reverse of the order they were opened" SAID
   s" the end tag a truncated document expects writes a tag character as U+XXXX" T-LABEL
   s\" <a\xF3\xA0\x81\x81>" XML:E-TRUNCATED BAD
   XML:E-TRUNCATED s" 1:8: expected `</aU+E0041>`, found the end of the input; write the rest of the document, closing each open element" SAID
   s" a document that ends inside an element names its end tag" T-LABEL
   s\" <a>\n<b>" XML:E-TRUNCATED BAD
   XML:E-TRUNCATED s" 2:4: expected `</b>`, found the end of the input; write the rest of the document, closing each open element" SAID
   s" an input reader places a rejection in the whole input after it drops consumed bytes" T-LABEL
   s\" <a>\n<b>xxxxxxxx</b>\n<c>yyyyyyyy</c>\n<d></e>\n</a>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 4:6: expected `</d>`, found `e`; close elements in the reverse of the order they were opened" SAID
   s" a found run that reaches the end of an input reader's window ends in ..." T-LABEL
   s" <a></a bbbbbbbbbbbb>" 3 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 1:8: expected `>`, found `bb...`; write that byte here, or correct the markup before it" SAID
   s" a found run that stops at a character the end of an input reader's window cuts ends in ..." T-LABEL
   s\" <a></a b\xC3\xA9bbbbbbb>" 3 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 1:8: expected `>`, found `b...`; write that byte here, or correct the markup before it" SAID
   s" a character the end of an input reader's window cuts is found as ..." T-LABEL
   s\" <a></a \xC3\xA9>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 1:8: expected `>`, found `...`; write that byte here, or correct the markup before it" SAID
   s" an input reader rejects a character XML 1.0 does not allow as a plain reader does" T-LABEL
   s\" <a>\n<b>xxxxxxxx</b>\n<c>yyyyyyyy</c>\n<d>\x01</d>\n</a>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-SCALAR T=
   XML:E-SCALAR s" 4:4: expected a character XML 1.0 allows, found byte $01; remove the character, or write one XML 1.0 allows" SAID
   s" an input reader rejects a bad character reference as a plain reader does" T-LABEL
   s\" <a>\n&#xZZ;</a>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-ENTITY T=
   XML:E-ENTITY s" 2:1: expected a character reference such as `&#65;` or `&#x41;`, found `&#xZ...`; write the character's number in decimal after `&#`, or in hex after `&#x`, and end the reference with `;`" SAID
   s" an input reader rejects a document cut inside a reference as a plain reader does" T-LABEL
   s\" <a>\n&am" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-TRUNCATED T=
   XML:E-TRUNCATED s" 2:4: expected the rest of the document, found the end of the input; write the rest of the document" SAID
   s" a found run that stops at bytes no character begins at the end of an input reader's window ends before them" T-LABEL
   s\" <a></a b\xC0\x80bbbbbbb>" 3 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 1:8: expected `>`, found `b`; write that byte here, or correct the markup before it" SAID
   s" a byte no character begins at the end of an input reader's window is found as that byte" T-LABEL
   s\" <a></a \xC0\x80>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-MALFORMED T=
   XML:E-MALFORMED s" 1:8: expected `>`, found byte $C0; write that byte here, or correct the markup before it" SAID
   s" a rejected reader rejects a further NEXT" T-LABEL
   s\" <a>\n&bogus;</a>" 2 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-OPEN
   ['] DRAIN catch XML:E-ENTITY T=
   XML:E-ENTITY s" 2:1: expected `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`, found `&bogus;`; write `&amp;` for a literal `&`, and end each reference with `;`" SAID
   ['] NEXT-ONE catch XML:E-STATE T=
   XML:CLOSE ;

: MARKUP-TEXTS ( -- )
   s" a start tag with no space between attributes" T-LABEL
   s" <a q='x'v='y'/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:9: expected white space, `>` or `/>` after the element name or an attribute, found `v`; separate attributes with white space, and end the start tag with `>` or `/>`" SAID
   s" a second root element" T-LABEL
   s" <a/><b/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:5: expected white space, a comment or a processing instruction after the root element, found `<`; wrap the elements in one root element" SAID
   s" an end tag with no open element" T-LABEL
   s" <a/></a>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:5: expected an end tag only after its start tag, found `<`; remove the end tag, or write its start tag before it" SAID
   s" `]]>` in text, after a newline and a two-byte character" T-LABEL
   s\" <a>\né]]></a>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 2:3: expected text without `]]>`, found `]]`; write `]]&gt;` for a literal `]]>` in text" SAID
   s" text after the root element" T-LABEL
   s" <a/>x" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:5: expected only white space outside the root element, found `x`; move the text inside the root element, or remove it" SAID
   s" `--` inside a comment" T-LABEL
   s" <!--a--b--><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:6: expected `-->` or comment text without `--`, found `--b--`; remove `--` from the comment, or end the comment with `-->`" SAID
   s" a CDATA section outside the root element" T-LABEL
   s" <![CDATA[x]]><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:1: expected a CDATA section only inside the root element, found `<`; move the CDATA section inside the root element, or remove it" SAID
   s" an XML declaration after the document's start" T-LABEL
   s"  <?xml version='1.0'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:2: expected `<?xml` only at the start of the document, found `<`; move the XML declaration to the start of the document, or remove it" SAID ;

\ The XML declaration's, a processing instruction's and `<!` markup's texts.
: PROLOG-TEXTS ( -- )
   s" an attribute after the declaration's last names the extra one" T-LABEL
   s" <?xml version='1.0' standalone='yes' foo='x'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:38: expected `?>` to end the XML declaration, found `foo`; write only `version`, `encoding` and `standalone`, in that order" SAID
   s" a misnamed last declaration attribute names it" T-LABEL
   s" <?xml version='1.0' foo='x'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:21: expected `standalone` as the declaration's last attribute, found `foo`; correct the attribute's name, or remove the attribute" SAID
   s" a standalone value other than yes or no names the value" T-LABEL
   s\" <?xml version='1.0'\nstandalone='maybe'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 2:13: expected `yes` or `no`, found `maybe`; write `yes` or `no` as `standalone`'s value" SAID
   s" a declaration not in lower case names its target" T-LABEL
   s" <?XML version='1.0'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:3: expected `xml` in lower case, found `XML`; begin the XML declaration with `<?xml`" SAID
   s" a document that ends inside the declaration asks for its end" T-LABEL
   s" <?xml version='1.0'" XML:E-TRUNCATED BAD
   XML:E-TRUNCATED s" 1:20: expected `?>` to end the XML declaration, found the end of the input; end the declaration with `?>`" SAID
   s" declaration attributes with no space between them" T-LABEL
   s" <?xml version='1.0'?x?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:20: expected white space or `?>`, found `?`; separate the declaration's attributes with a space" SAID
   s" a document that ends after a processing instruction's target" T-LABEL
   s" <?p" XML:E-TRUNCATED BAD
   XML:E-TRUNCATED s" 1:4: expected `?>` to end the processing instruction, found the end of the input; end the processing instruction with `?>`" SAID
   s" a target joined to its content counts columns in bytes" T-LABEL
   s\" <a>\n\xC3\xA9<?p/?></a>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 2:6: expected white space or `?>` after the target, found `/`; put a space between the target and the instruction's content" SAID
   s" a DOCTYPE is rejected where it starts" T-LABEL
   s" <!DOCTYPE a><a/>" XML:E-DTD BAD
   XML:E-DTD s" 1:1: expected an element, a comment or a processing instruction, found `<`; remove the `<!DOCTYPE`: the reader reads no DTD" SAID
   s" `<!` that opens neither a comment nor CDATA" T-LABEL
   s" <a><!x></a>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:4: expected `<!--` or `<![CDATA[`, found `<`; write `<!--` for a comment or `<![CDATA[` for character data" SAID ;

: NAME-TEXTS ( -- )
   s" a name that starts with neither a letter, `_` nor `:` is rejected at it" T-LABEL
   s\" <a>\n<\xC3\xA9 1='x'/></a>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 2:5: expected a name, found `1`; start the name with a letter, `_` or `:`" SAID
   s" a name that starts with `:` is rejected at the name" T-LABEL
   s" <:a/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:2: expected a name that starts with a letter or `_`, found `:a`; write a prefix before the leading `:`, or remove it" SAID
   s" a name with a second `:` is rejected at the name" T-LABEL
   s" <p:a:b/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:2: expected a name with at most one `:`, between a prefix and a local name, found `p:a:b`; write the name as `prefix:local` or `local`" SAID
   s" an attribute whose local name starts with a digit is rejected at the attribute's name" T-LABEL
   s" <a p:1='x'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:4: expected a local name after `:` that starts with a letter or `_`, found `p:1`; start the part after `:` with a letter or `_`" SAID
   s" an unquoted attribute value is rejected at it" T-LABEL
   s" <a b=c/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s\" 1:6: expected a value in `\"` or `'` quotes, found `c`; write the value between `\"` or `'` quotes" SAID
   s" a `<` in an attribute value is rejected at it" T-LABEL
   s" <a q='<x'/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s" 1:7: expected the attribute value's closing quote, found `<`; close the value with its quote, or write `&lt;` for a literal `<`" SAID ;

: NAMESPACE-TEXTS ( -- )
   s" a declaration of the xmlns prefix is rejected at its name" T-LABEL
   s" <a xmlns:xmlns='u'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:4: expected a prefix other than `xmlns`, found `xmlns:xmlns`; remove the declaration: XML binds `xmlns` itself" SAID
   s" a binding of the xmlns namespace is rejected at its name" T-LABEL
   s" <a xmlns:p='http://www.w3.org/2000/xmlns/'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:4: expected a namespace other than `http://www.w3.org/2000/xmlns/`, found `xmlns:p`; declare another namespace URI, or remove the declaration" SAID
   s" the xml prefix bound to another namespace is rejected at its name" T-LABEL
   s" <a xmlns:xml='other'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:4: expected `xml` bound to `http://www.w3.org/XML/1998/namespace`, found `xmlns:xml`; remove the declaration: XML binds `xml` itself" SAID
   s" the XML namespace bound to another prefix is rejected at its name" T-LABEL
   s" <a xmlns='http://www.w3.org/XML/1998/namespace'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:4: expected a namespace other than `http://www.w3.org/XML/1998/namespace`, found `xmlns`; write the `xml` prefix for that namespace, and remove this declaration" SAID
   s" an empty prefix declaration counts the line and the bytes of the column" T-LABEL
   s\" <r>\n\xC3\xA9<a xmlns:p=''/></r>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 2:6: expected a prefix declaration with a URI, found `xmlns:p`; write the prefix's URI: XML 1.0 cannot undeclare a prefix" SAID
   s" a repeated attribute is rejected at the second one" T-LABEL
   s" <a q='1' q='2'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:10: expected an attribute the element does not already have, found `q`; remove the repeated attribute, or give it another name or namespace" SAID
   s" a repeated expanded attribute name is rejected at the second one" T-LABEL
   s" <a xmlns:p='u' xmlns:q='u' p:x='1' q:x='2'/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:36: expected an attribute the element does not already have, found `q:x`; remove the repeated attribute, or give it another name or namespace" SAID
   s" an input reader rejects an unbound prefix at the name in the whole input" T-LABEL
   s\" <r>\n<a>\n<p:b/></a></r>" 2 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE XML:E-NAMESPACE T=
   XML:E-NAMESPACE s" 3:2: expected a prefix an `xmlns:` declaration binds in scope, found `p:b`; declare it with `xmlns:` on this element or an enclosing one" SAID
   s" an element name with the xmlns prefix is rejected at the name" T-LABEL
   s" <xmlns:a/>" XML:E-NAMESPACE BAD
   XML:E-NAMESPACE s" 1:2: expected an element name without the `xmlns` prefix, found `xmlns:a`; rename the element: `xmlns` marks only namespace declarations" SAID ;


: DECLARATION-TEXTS ( -- )
   s" a declaration without attributes names `version` where it is missing" T-LABEL
   s" <?xml?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s\" 1:6: expected `version=\q1.0\q`, found `?`; write `version=\q1.0\q` after `<?xml`" SAID
   s" a declaration whose first attribute is not `version` names that attribute" T-LABEL
   s" <?xml encoding='UTF-8' version='1.0'?><a/>" XML:E-MALFORMED BAD
   XML:E-MALFORMED s\" 1:7: expected `version` as the first attribute, found `encoding`; write `version=\q1.0\q` before the declaration's other attributes" SAID
   s" a version other than 1.0 is rejected at its value" T-LABEL
   s" <?xml version='1.1'?><a/>" XML:E-ENCODING BAD
   XML:E-ENCODING s\" 1:16: expected `1.0`, found `1.1`; declare `version=\q1.0\q`: the reader reads XML 1.0" SAID
   s" an input reader counts a byte order mark's three bytes in the column" T-LABEL
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-FAILURE XML:E-ENCODING T=
   XML:E-ENCODING s\" 1:34: expected `UTF-8` after a UTF-8 byte order mark, found `US-ASCII`; declare `encoding=\qUTF-8\q`, or remove the byte order mark" SAID
   s" US-ASCII after a byte order mark is rejected at the value on its line" T-LABEL
   s\" \xEF\xBB\xBF<?xml\nversion='1.0' encoding='US-ASCII'?><a/>" XML:E-ENCODING BAD
   XML:E-ENCODING s\" 2:25: expected `UTF-8` after a UTF-8 byte order mark, found `US-ASCII`; declare `encoding=\qUTF-8\q`, or remove the byte order mark" SAID
   s" a plain reader takes UTF-8 or US-ASCII only" T-LABEL
   s" <?xml version='1.0' encoding='ISO-8859-1'?><a/>" XML:E-ENCODING BAD
   XML:E-ENCODING s\" 1:31: expected `UTF-8` or `US-ASCII`, found `ISO-8859-1`; save the document as UTF-8 and declare `encoding=\qUTF-8\q`" SAID ;

: SCALAR-TEXTS ( -- )
   s" a character XML 1.0 does not allow is rejected where it stands" T-LABEL
   s\" <a>\n\x01</a>" XML:E-SCALAR BAD
   XML:E-SCALAR s" 2:1: expected a character XML 1.0 allows, found byte $01; remove the character, or write one XML 1.0 allows" SAID
   s" a reference to a character XML 1.0 does not allow is rejected at its `&`" T-LABEL
   s" <a>&#1;</a>" XML:E-SCALAR BAD
   XML:E-SCALAR s" 1:4: expected a character XML 1.0 allows, found `&#1;`; remove the character, or write one XML 1.0 allows" SAID
   s" a reference to no predefined entity is rejected at its `&`" T-LABEL
   s" <a>&foo;</a>" XML:E-ENTITY BAD
   XML:E-ENTITY s" 1:4: expected `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`, found `&foo;`; write `&amp;` for a literal `&`, and end each reference with `;`" SAID
   s" a character reference with a digit outside its radix is rejected at its `&`" T-LABEL
   s" <a>&#xZZ;</a>" XML:E-ENTITY BAD
   XML:E-ENTITY s" 1:4: expected a character reference such as `&#65;` or `&#x41;`, found `&#xZZ;`; write the character's number in decimal after `&#`, or in hex after `&#x`, and end the reference with `;`" SAID
   s" a byte no UTF-8 character starts counts a two-byte character's bytes in the column" T-LABEL
   s\" <a>\n\xC3\xA9\xFF</a>" XML:E-UTF8 BAD
   XML:E-UTF8 s" 2:3: expected a UTF-8 character, found byte $FF; write the text in UTF-8" SAID
   s" ESCAPE-TEXT places bad UTF-8 in the text it is given" T-LABEL
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ESCAPED-OUT !
   [: s\" ab\xFF" ESCAPED-OUT @ XML:ESCAPE-TEXT ;] XML:E-UTF8 TTHROWSQ
   ESCAPED-OUT @ VEC:DELETE
   XML:E-UTF8 s" 1:3: expected a UTF-8 character, found byte $FF; write the text in UTF-8" SAID
   s" a US-ASCII input stream places a high byte of a later chunk in the whole input" T-LABEL
   s\" <?xml version='1.0' encoding='US-ASCII'?>\n<a>\xC3\xA9</a>"
   1 $7FFFFFFFFFFFFFFF INPUT-SET INPUT-FAILURE XML:E-ENCODING T=
   XML:E-ENCODING s\" 2:4: expected a US-ASCII character, found `...`; write it as a character reference such as `&#233;`, or save the document as UTF-8 and declare `encoding=\qUTF-8\q`" SAID
   s" a US-ASCII input stream places a high byte its first chunk holds" T-LABEL
   s\" <?xml version='1.0' encoding='US-ASCII'?>\n<a>\xC3\xA9</a>"
   $7FFFFFFFFFFFFFFF $7FFFFFFFFFFFFFFF INPUT-SET INPUT-FAILURE XML:E-ENCODING T=
   XML:E-ENCODING s\" 2:4: expected a US-ASCII character, found `\xC3\xA9`; write it as a character reference such as `&#233;`, or save the document as UTF-8 and declare `encoding=\qUTF-8\q`" SAID ;

DECODED-NEW
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
TEXT-APPENDS
PREFIX-RECOVERY
PREFIX-READERS
ABANDONED-RECORD
FAILED-BUILDS
WIDE-ELEMENT
ESCAPING
CHECKED-OWNERSHIP
INPUT-BOUNDARIES
INPUT-MARKUP
INPUT-NEWLINE
3 INPUT-LARGE-TEXT
$7FFFFFFFFFFFFFFF INPUT-LARGE-TEXT
INPUT-LARGE-SCOPE
INPUT-NAMESPACE-SCOPES
INPUT-DEEP-CHAIN
INPUT-FAILURES
REJECTION-TEXTS
MARKUP-TEXTS
PROLOG-TEXTS
NAME-TEXTS
NAMESPACE-TEXTS
DECLARATION-TEXTS
SCALAR-TEXTS
TALL-TREE
MANY-ATTRIBUTES
MANY-BINDINGS
DECODED @ VEC:DELETE
T-REPORT
;package
