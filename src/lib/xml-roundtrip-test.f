split-str
require src/lib/test.f
require src/lib/xml.f
require src/lib/byte-edit.f
require src/lib/property.f
require src/lib/vector.f
require src/lib/heap.f

package XML-ROUNDTRIP-TEST
using XML
CAST: TOKEN>N ( XML:kind -- n )
$1000 constant BYTE-CAP
align
create EDITOR 2 EDIT:STORAGE-BYTES allot
create DOCUMENT BYTE-CAP allot
TYPED-VARIABLE READBACK ^VEC:vector<u8>
create SINGLE 1 allot
create RANDOM-BUF BYTE-CAP allot
variable TEMPLATE
TYPED-VARIABLE RD XML:cursor

: ORIGINAL$ ( -- str )
   TEMPLATE @ case
      0 of s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q='old'>old</p:a><!--keep-->" endof
      1 of s\" <?xml version=\"1.0\"?><p:a xmlns:p=\"urn:p\" q=\"&amp;\">&#65;</p:a><!--keep-->" endof
      2 of s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q = '&#x1F600;' >é</p:a><!--keep-->" endof
      s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q=''>long &lt; old</p:a><!--keep-->" rot
   endcase ;

: OPEN ( ^u8 n -- XML:reader )
   {: source size:n :}
   RD source size INIT ;

: ATTRIBUTE-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   ATTR-VALUE {: start:off size:len :}
   XML:CLOSE start size ;

: TEXT-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop NEXT drop
   CONTENT {: start:off size:len :}
   XML:CLOSE start size ;

: APPLY ( ^u8 n -- n )
   {: value size:n :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: attr:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: text:^ :}
   value size attr ESCAPE-ATTR
   value size text ESCAPE-TEXT
   ATTRIBUTE-RANGE {: attr-start:off attr-old:len :}
   TEXT-RANGE {: text-start:off text-old:len :}
   EDITOR 2 EDIT:STORAGE-BYTES ORIGINAL$ EDIT:INIT
   attr-start attr-old attr VEC:SPAN SPAN:$ EDIT:REPLACE
   text-start text-old text VEC:SPAN SPAN:$ EDIT:REPLACE
   DOCUMENT BYTE-CAP EDIT:WRITE {: document-size:n :}
   EDIT:CLOSE
   attr VEC:DELETE  text VEC:DELETE
   document-size ;

: READBACK-NEW ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW READBACK ! ;
: CLEARED ( -- ^VEC:vector<u8> ) READBACK @ dup 0 VEC:RESIZE ;
: READBACK$ ( -- str ) READBACK @ VEC:SPAN SPAN:$ ;

: VERIFY ( ^u8 n n -- )
   {: value size:n document-size:n :}
   DOCUMENT document-size OPEN
   NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   CLEARED ATTR-TEXT READBACK$ value size T$=
   NEXT drop
   CLEARED TEXT READBACK$ value size T$=
   NEXT drop NEXT drop
   CLEARED TEXT READBACK$ s" keep" T$=
   NEXT drop XML:CLOSE ;

: ROUNDTRIP ( ^u8 n -- )
   {: value size:n :}
   value size value size APPLY VERIFY ;

: ASCII-ROUNDTRIPS ( -- )
   $7F $20 ?do
      i SINGLE c!
      SINGLE 1 ROUNDTRIP
   loop ;

\ Chunks include every XML escape, whitespace normalization, multi-byte
\ offsets, combining text, and expanding Unicode casefold inputs.
: CHUNK$ ( -- str )
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

: RANDOM-VALUE$ ( -- str )
   0
   32 PROP:RND% 1+ 0 ?do
      {: size:n :}
      CHUNK$ {: chunk:^ count:n :}
      chunk RANDOM-BUF size + count BYTE-COPY
      size count +
   loop
   RANDOM-BUF swap ;

: DRAIN ( XML:reader -- XML:reader )
   begin NEXT TOKEN>N XML-KIND:EOF TOKEN>N <> while repeat ;

: NEXT-AFTER ( XML:reader -- XML:reader )
   NEXT drop ;

\ The body is a word reached by `[']`, not a quotation literal: the reader is read
\ and closed after the catch, `catch` restores stack depth but not contents, and
\ a literal would leave stale<XML:reader>, which can be neither read nor dropped.
: REJECTS ( ^u8 n -- )
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

READBACK-NEW
T-RESET
0 TEMPLATE !
ASCII-ROUNDTRIPS
s\" \t\n\r" ROUNDTRIP
s" <&>'é名😀🚀" ROUNDTRIP
RANDOM-ROUNDTRIPS
READBACK @ VEC:DELETE
T-REPORT
;using
;package
