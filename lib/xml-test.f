require lib/test.f
require lib/xml.f

package XML-TEST
using XML

64 constant CAP
$400 constant TEXT-CAP
$31415926 constant CANARY
here CELL 1- and CELL swap - CELL 1- and allot
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

\ ---- the caught bodies of the recovery cases --------------------------------
\ Every case below reads the reader after the catch, so the handle must SURVIVE
\ the caught failure. A quotation literal leaves it `stale<XML:reader>` - `catch`
\ restores the DEPTH of both stacks and never their contents - and a linear cell
\ can be neither read nor dropped, so each body is a name reached by `[']`: an
\ exceptional edge is not part of a quotation's TYPE, so that route keeps the
\ window typed until the callee-evidence lane (dot c2923193) lets the checker
\ prove the handle intact.
\ ATTR-RESET is XML's own word and needs no body of its own, but `['] ATTR-RESET`
\ on the bare name a `using` import resolved is a SIGSEGV at run time - on this
\ engine and on the one before this lane. `['] XML:ATTR-RESET` runs, and so does
\ this wrapper, which is the shape of every other body here.
: ATTR-RESET-ONE ( XML:reader -- XML:reader ) ATTR-RESET ;
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
   ['] ATTR-RESET-ONE catch E-STATE T=
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
   [: s\" \z" TEXT-BUF TEXT-CAP ESCAPE-TEXT drop ;] E-SCALAR TTHROWSQ ;

: CHECKED-OWNERSHIP ( -- )
   s" XML-TEST-DUP ( XML:reader -- XML:reader XML:reader ) dup" CHECK! 0 T=
   s" XML-TEST-DROP ( XML:reader -- ) drop" CHECK! 0 T=
   s" XML-TEST-OFFSET ( XML:reader -- XML:reader len off ) XML:RAW" CHECK! 0 T= ;

T-RESET
EMPTY-ELEMENT
NAMESPACE-SCOPES
NAMESPACE-ENTITIES
SCALARS-AND-OFFSETS
ATTRIBUTES-AND-SPANS
MARKUP-TEXT
UNICODE-NAMES
MALFORMED-INPUT
BAD-ENTITIES
BAD-NAMESPACES
BAD-ENCODINGS
CAPACITY-AND-STATE
CALLER-SIZED-STORAGE
NO-PARTIAL-DECODE
URI-CAPACITY-RECOVERY
ATTRIBUTE-CAPACITY-RECOVERY
ESCAPING
CHECKED-OWNERSHIP
BEFORE @ CANARY T=
AFTER @ CANARY T=
T-REPORT
;using
;package
