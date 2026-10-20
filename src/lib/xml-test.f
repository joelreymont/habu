require src/lib/test.f
require src/lib/xml.f
require src/lib/test/guard-page.f

package XML-TEST
using XML

64 constant CAP
$400 constant TEXT-CAP
$31415926 constant CANARY
align
create BEFORE CANARY ,
create STORAGE CAP STORAGE-BYTES allot
create AFTER CANARY ,
create TINY 1 STORAGE-BYTES allot
create SMALL 2 STORAGE-BYTES allot
create LARGE 128 STORAGE-BYTES allot
create TEXT-BUF TEXT-CAP allot
create OTHER-BUF TEXT-CAP allot
create NESTED $1000 allot
variable NESTED-LEN
16 constant INPUT-CAP
create INPUT-STORAGE INPUT-CAP STORAGE-BYTES allot
3 SPAN-BUFFER: INPUT-CHUNK
PTR-VARIABLE INPUT-SOURCE
variable INPUT-LEN
variable INPUT-POS
variable INPUT-LIMIT
variable INPUT-FAIL
variable INPUT-OFF
variable INPUT-USED
create INPUT-BIG 8200 allot

CAST: TOKEN>N ( XML:kind -- n )

: OPEN ( ptr u8 n -- XML:reader )
   STORAGE CAP STORAGE-BYTES 2swap INIT ;

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
   INPUT-POS ['] INPUT-FILL INPUT-CHUNK
   INPUT-STORAGE INPUT-CAP STORAGE-BYTES INIT-INPUT ;

: INPUT-FAILURE ( -- n )
   INPUT-OPEN ['] DRAIN catch swap XML:CLOSE ;

: INPUT-OVER-COUNT ( ptr n SPAN:span<u8> -- n )
   2drop 4 ;

: INPUT-OVER-COUNT-RC ( -- n )
   INPUT-POS ['] INPUT-OVER-COUNT INPUT-CHUNK
   INPUT-STORAGE INPUT-CAP STORAGE-BYTES INIT-INPUT
   ['] DRAIN catch swap XML:CLOSE ;

: INPUT-TEXT-ALIASED ( XML:reader -- XML:reader )
   INPUT-CHUNK SPAN:$ TEXT drop ;

: INPUT-BAD ( ptr u8 n n -- )
   {: source size:n expected:n :}
   source size 1 $7FFFFFFFFFFFFFFF INPUT-SET
   INPUT-FAILURE expected T= ;

\ The reader is opened INSIDE the caught body, the way BAD-INIT below already
\ does it: `catch` restores the DEPTH of both stacks and never their contents, so
\ a reader opened before the catch comes back `stale<XML:reader>` and a linear
\ cell can be neither read nor dropped. What the throw path leaves open here is
\ the file's own static storage, which the next OPEN re-initialises.
: BAD ( ptr u8 n n -- )
   {: expected:n :}
   [: 2dup OPEN DRAIN XML:CLOSE ;] catch expected T=
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
   s" <a xmlns:p='u' xmlns:q='&#117;' p:x='1' q:x='2'/>" E-NAMESPACE BAD
   s" <a xmlns:p=''/>" E-NAMESPACE BAD
   s" <a xmlns:xml='other'/>" E-NAMESPACE BAD
   s" <a xmlns:xmlns='u'/>" E-NAMESPACE BAD
   s" <a xmlns='http://www.w3.org/XML/1998/namespace'/>" E-NAMESPACE BAD
   s" <a xmlns:p='http://www.w3.org/2000/xmlns/'/>" E-NAMESPACE BAD ;

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

\ ---- the caught bodies of the recovery cases --------------------------------
\ Every case below reads the reader after the catch, so the handle must SURVIVE
\ the caught failure. A quotation literal leaves it `stale<XML:reader>` - `catch`
\ restores the DEPTH of both stacks and never their contents - and a linear cell
\ can be neither read nor dropped, so each body is a name reached by `[']`: an
\ exceptional edge is not part of a quotation's TYPE, so that route keeps the
\ window typed until the callee-evidence lane (dot c2923193) lets the checker
\ prove the handle intact.
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
: TEXT-ALIASED ( XML:reader -- XML:reader ) STORAGE BYTE-VIEW TEXT-CAP TEXT drop ;
: URI-TINY ( XML:reader -- XML:reader ) TEXT-BUF 4 URI drop ;
: URI-ALIASED ( XML:reader -- XML:reader ) STORAGE BYTE-VIEW TEXT-CAP URI drop ;
: ATTR-URI-TINY ( XML:reader -- XML:reader ) TEXT-BUF 4 ATTR-URI drop ;
: ATTR-URI-ALIASED ( XML:reader -- XML:reader ) STORAGE BYTE-VIEW TEXT-CAP ATTR-URI drop ;
: ATTR-TEXT-TINY ( XML:reader -- XML:reader ) TEXT-BUF 1 ATTR-TEXT drop ;
: ATTR-TEXT-ALIASED ( XML:reader -- XML:reader ) STORAGE BYTE-VIEW TEXT-CAP ATTR-TEXT drop ;
: PREFIX-TINY ( XML:reader -- XML:reader )
   s" p" TEXT-BUF 2 PREFIX-URI 2drop ;
: PREFIX-ALIASED ( XML:reader -- XML:reader )
   s" p" STORAGE BYTE-VIEW TEXT-CAP PREFIX-URI 2drop ;

: CAPACITY-AND-STATE ( -- )
   TINY 1 STORAGE-BYTES s" <a><b/></a>" INIT
   ['] DRAIN catch E-DEPTH T=
   ['] NEXT-ONE catch E-STATE T=
   XML:CLOSE
   TINY 1 STORAGE-BYTES s" <a q='1' r='2'/>" INIT
   ['] DRAIN catch E-ATTRIBUTES T= XML:CLOSE
   SMALL 2 STORAGE-BYTES s" <a xmlns:p='u' xmlns:q='v'><b xmlns:r='w'/></a>" INIT
   ['] DRAIN catch E-NAMESPACES T= XML:CLOSE
   s" <a xmlns:p='u' xmlns:q='v'><b xmlns:r='w'/></a>" OPEN DRAIN XML:CLOSE
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

: NESTED+ ( ptr u8 n -- )
   {: source size:n :}
   source NESTED NESTED-LEN @ + size >LEN BYTE-COPY-LEN
   size NESTED-LEN +! ;

: CALLER-SIZED-STORAGE ( -- )
   0 NESTED-LEN !
   96 0 ?do s" <p:a xmlns:p='u'>" NESTED+ loop
   96 0 ?do s" </p:a>" NESTED+ loop
   LARGE 128 STORAGE-BYTES NESTED NESTED-LEN @ INIT
   DRAIN XML:CLOSE ;

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
   TINY 1 STORAGE-BYTES s" <a><b/></a>" INIT
   ['] DRAIN catch E-DEPTH T=
   ['] PREFIX-READ catch E-STATE T=
   XML:CLOSE ;

: PREFIX-READERS ( -- )
   s" <a xmlns:p='urn:first'/>" OPEN
   LARGE 128 STORAGE-BYTES s" <b xmlns:p='urn:second'/>" INIT
   XML-KIND:START NEXT= s" p" s" urn:second" PREFIX=
   swap XML-KIND:START NEXT= s" p" s" urn:first" PREFIX=
   swap s" p" s" urn:second" PREFIX=
   XML:CLOSE XML:CLOSE ;

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
BAD-ENCODINGS
ASCII-WIRE
CAPACITY-AND-STATE
CALLER-SIZED-STORAGE
NO-PARTIAL-DECODE
URI-CAPACITY-RECOVERY
PREFIX-RECOVERY
PREFIX-READERS
ATTRIBUTE-CAPACITY-RECOVERY
ESCAPING
CHECKED-OWNERSHIP
INPUT-BOUNDARIES
INPUT-MARKUP
INPUT-NEWLINE
INPUT-ALIAS-CHECK
INPUT-LARGE-TEXT
INPUT-LARGE-SCOPE
INPUT-FAILURES
BEFORE @ CANARY T=
AFTER @ CANARY T=
T-REPORT
;using
;package
