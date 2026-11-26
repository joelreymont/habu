require src/lib/test/assert.f
require src/lib/xml/read.f
require src/lib/xml/scalar.f
require src/lib/xml/state.f
require src/lib/xml/write.f
require src/lib/xml.f
require src/lib/byte-edit.f
require src/lib/property.f
require src/lib/vector.f
require src/lib/heap.f

package XML-ROUNDTRIP-TEST
CAST: TOKEN>N ( XML:kind -- n )
$1000 constant BYTE-CAP
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
      s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q=''>long &lt; old</p:a><!--keep-->" swap
   endcase ;

: OPEN ( str -- XML:reader )
   {: source:str :}
   RD HEAP:ALLOCATOR source XML:INIT ;

: ATTRIBUTE-RANGE ( -- off len )
   ORIGINAL$ OPEN XML:NEXT drop XML:NEXT drop
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE {: start:off size:len :}
   XML:CLOSE start size ;

: TEXT-RANGE ( -- off len )
   ORIGINAL$ OPEN XML:NEXT drop XML:NEXT drop XML:NEXT drop
   XML:CONTENT {: start:off size:len :}
   XML:CLOSE start size ;

: APPLY ( str -- n )
   {: value:str :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: attr:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: text:^ :}
   value attr XML:ESCAPE-ATTR
   value text XML:ESCAPE-TEXT
   ATTRIBUTE-RANGE {: attr-start:off attr-old:len :}
   TEXT-RANGE {: text-start:off text-old:len :}
   HEAP:ALLOCATOR ORIGINAL$ EDIT:INIT
   attr-start attr-old attr VEC:BYTES EDIT:REPLACE
   text-start text-old text VEC:BYTES EDIT:REPLACE
   DOCUMENT BYTE-CAP EDIT:WRITE {: document-size:n :}
   EDIT:CLOSE
   attr VEC:DELETE  text VEC:DELETE
   document-size ;

: READBACK-NEW ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW READBACK ! ;
: CLEARED ( -- ^VEC:vector<u8> ) READBACK @ dup 0 VEC:RESIZE ;
: READBACK$ ( -- str ) READBACK @ VEC:BYTES ;

: VERIFY ( str n -- )
   {: value:str document-size:n :}
   DOCUMENT document-size >str OPEN
   XML:NEXT drop XML:NEXT drop
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT READBACK$ value T$=
   XML:NEXT drop
   CLEARED XML:TEXT READBACK$ value T$=
   XML:NEXT drop XML:NEXT drop
   CLEARED XML:TEXT READBACK$ s" keep" T$=
   XML:NEXT drop XML:CLOSE ;

: ROUNDTRIP ( str -- )
   {: value:str :}
   value value APPLY VERIFY ;

: ASCII-ROUNDTRIPS ( -- )
   $7F $20 ?do
      i SINGLE c!
      SINGLE 1 >str ROUNDTRIP
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
      s" abc 019" swap
   endcase ;

: RANDOM-VALUE$ ( -- str )
   0
   32 PROP:RND% 1+ 0 ?do
      {: size:n :}
      CHUNK$ {: chunk:str :}
      chunk STR:DATA RANDOM-BUF size + chunk STR:LEN BYTE-COPY
      size chunk STR:LEN +
   loop
   RANDOM-BUF swap >str ;

: DRAIN ( XML:reader -- XML:reader )
   begin XML:NEXT TOKEN>N XML-KIND:EOF TOKEN>N <> while repeat ;

: NEXT-AFTER ( XML:reader -- XML:reader )
   XML:NEXT drop ;

\ The body is a word reached by `[']`, not a quotation literal: the reader is read
\ and closed after the catch, `catch` restores stack depth but not contents, and
\ a literal would leave stale<XML:reader>, which can be neither read nor dropped.
: REJECTS ( str -- )
   OPEN ['] DRAIN catch 0<> TTRUE
   ['] NEXT-AFTER catch XML:E-STATE T=
   XML:CLOSE ;

: RANDOM-REJECTION ( n -- )
   \ The final 17 bytes close the root and retain its following comment.
   \ Every selected prefix stops before that close, including mid-UTF8,
   \ mid-entity, mid-attribute, and incomplete processing-instruction cuts.
   17 - PROP:RND% 1+ DOCUMENT swap >str REJECTS ;

: RANDOM-ROUNDTRIPS ( -- )
   $517A32D PROP:SEED!
   2048 0 ?do
      4 PROP:RND% TEMPLATE !
      RANDOM-VALUE$ dup APPLY dup >r VERIFY r> RANDOM-REJECTION
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
;package
