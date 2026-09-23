require lib/test.f
require lib/xml.f
require lib/byte-edit.f
require lib/property.f

package XML-ROUNDTRIP-TEST
using XML
CAST: TOKEN>N ( XML:kind -- n )
8 constant XML-CAP
$1000 constant BYTE-CAP
here CELL 1- and CELL swap - CELL 1- and allot
create PARSER XML-CAP STORAGE-BYTES allot
create EDITOR 2 EDIT:STORAGE-BYTES allot
create ATTRIBUTE-BUF BYTE-CAP allot
create TEXT-BUF BYTE-CAP allot
create DOCUMENT BYTE-CAP allot
create READBACK BYTE-CAP allot
create SINGLE 1 allot
create RANDOM-BUF BYTE-CAP allot
variable TEMPLATE

: ORIGINAL$ ( -- ptr u8 n )
   TEMPLATE @ case
      0 of s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q='old'>old</p:a><!--keep-->" endof
      1 of s\" <?xml version=\"1.0\"?><p:a xmlns:p=\"urn:p\" q=\"&amp;\">&#65;</p:a><!--keep-->" endof
      2 of s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q = '&#x1F600;' >é</p:a><!--keep-->" endof
      s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q=''>long &lt; old</p:a><!--keep-->" rot
   endcase ;

: OPEN ( ptr u8 n -- XML:reader )
   PARSER XML-CAP STORAGE-BYTES 2swap INIT ;

: ATTRIBUTE-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   ATTR-VALUE {: start:off size:len :}
   XML:CLOSE start size ;

: TEXT-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop NEXT drop
   CONTENT {: start:off size:len :}
   XML:CLOSE start size ;

: APPLY ( ptr u8 n -- n )
   {: value size:n :}
   value size ATTRIBUTE-BUF BYTE-CAP ESCAPE-ATTR {: attr-size:n :}
   value size TEXT-BUF BYTE-CAP ESCAPE-TEXT {: text-size:n :}
   ATTRIBUTE-RANGE {: attr-start:off attr-old:len :}
   TEXT-RANGE {: text-start:off text-old:len :}
   EDITOR 2 EDIT:STORAGE-BYTES ORIGINAL$ EDIT:INIT
   attr-start attr-old ATTRIBUTE-BUF attr-size EDIT:REPLACE
   text-start text-old TEXT-BUF text-size EDIT:REPLACE
   DOCUMENT BYTE-CAP EDIT:WRITE {: document-size:n :}
   EDIT:CLOSE document-size ;

: VERIFY ( ptr u8 n n -- )
   {: value size:n document-size:n :}
   DOCUMENT document-size OPEN
   NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   READBACK BYTE-CAP ATTR-TEXT READBACK swap value size T$=
   NEXT drop
   READBACK BYTE-CAP TEXT READBACK swap value size T$=
   NEXT drop NEXT drop
   READBACK BYTE-CAP TEXT READBACK swap s" keep" T$=
   NEXT drop XML:CLOSE ;

: ROUNDTRIP ( ptr u8 n -- )
   {: value size:n :}
   value size value size APPLY VERIFY ;

: ASCII-ROUNDTRIPS ( -- )
   $7F $20 ?do
      i SINGLE c!
      SINGLE 1 ROUNDTRIP
   loop ;

\ Chunks include every XML escape, whitespace normalization, multi-byte
\ offsets, combining text, and expanding Unicode casefold inputs.
: CHUNK$ ( -- ptr u8 n )
   12 PROP:RND% case
      0 of s" <" endof
      1 of s" &" endof
      2 of s" >" endof
      3 of s" '" endof
      4 of s\" \"" endof
      5 of s\" \t\r\n" endof
      6 of s" éУКР" endof
      7 of s" Straße" endof
      8 of s" İi̇" endof
      9 of s" 😀🚀" endof
      10 of s" 名" endof
      s" abc 019" rot
   endcase ;

: RANDOM-VALUE$ ( -- ptr u8 n )
   0
   32 PROP:RND% 1+ 0 ?do
      {: size:n :}
      CHUNK$ {: chunk:ptr count:n :}
      chunk RANDOM-BUF size + count BYTE-COPY
      size count +
   loop
   RANDOM-BUF swap ;

: DRAIN ( XML:reader -- XML:reader )
   begin NEXT TOKEN>N XML-KIND:EOF TOKEN>N <> while repeat ;

: NEXT-AFTER ( XML:reader -- XML:reader )
   NEXT drop ;

\ The reader must survive the caught failure: this case reads it afterwards and
\ closes it. A quotation literal leaves it `stale<XML:reader>` - `catch` restores
\ the DEPTH of both stacks and never their contents - and a linear cell can be
\ neither read nor dropped, so the body is named and reached by `[']`: the
\ exceptional edge is not part of a quotation's TYPE, so that route keeps the
\ window typed until the callee-evidence lane (dot c2923193) lets the checker
\ prove the handle intact.
: REJECTS ( ptr u8 n -- )
   OPEN ['] DRAIN catch 0<> TTRUE
   ['] NEXT-AFTER catch E-STATE T=
   XML:CLOSE ;

: RANDOM-REJECTION ( n -- )
   \ The final 17 bytes close the root and retain its following comment.
   \ Every selected prefix stops before that close, including mid-UTF8,
   \ mid-entity, mid-attribute, and incomplete processing-instruction cuts.
   17 - PROP:RND% 1+ DOCUMENT swap REJECTS ;

: RANDOM-ROUNDTRIPS ( -- )
   $517A32D PROP:SEED!
   2048 0 ?do
      4 PROP:RND% TEMPLATE !
      RANDOM-VALUE$ 2dup APPLY dup >r VERIFY r> RANDOM-REJECTION
   loop ;

T-RESET
0 TEMPLATE !
ASCII-ROUNDTRIPS
s\" \t\n\r" ROUNDTRIP
s" <&>'é名😀🚀" ROUNDTRIP
RANDOM-ROUNDTRIPS
T-REPORT
;using
;package
