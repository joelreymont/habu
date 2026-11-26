\ png-test.f - PNG:DECODE-RGB on PNGs the test assembles: chunks framed and
\ checked with ZLIB, rows behind filter byte 0.
\ Run: bin/hb --load src/lib/png-test.f

require src/lib/test/assert.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/be.f
require src/lib/zlib.f
require src/lib/png.f

package PNG-TEST
private

2 constant COLOUR-RGB
4 constant COLOUR-GREY-ALPHA

: FRESH ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;
: SIGNATURE$ ( -- str ) s\" \x89PNG\r\n\x1A\n" ;
\ Bit depth 8.
: DEPTH$ ( -- str ) s\" \x08" ;
\ Compression, filter and interlace methods 0.
: METHODS$ ( -- str ) s\" \x00\x00\x00" ;

: U32+ ( n ^VEC:vector<u8> -- )
   {: x:n v:^ :}
   x v 4 VEC:EXTEND SPAN:$ STR:DATA BE:U32! ;

: BYTE+ ( n ^VEC:vector<u8> -- )
   {: x:n v:^ :}
   x v 1 VEC:EXTEND SPAN:$ STR:DATA c! ;

\ Length, type, data, and the CRC-32 of type and data.
: CHUNK ( str str ^VEC:vector<u8> -- )
   {: kind:str data:str v:^ :}
   data STR:LEN v U32+
   kind v VEC:APPEND  data v VEC:APPEND
   kind ZLIB:CRC32 data ZLIB:CRC32-FEED v U32+ ;

: IHDR ( n n n ^VEC:vector<u8> -- )
   {: w:n h:n colour:n v:^ :}
   FRESH {: hdr:^ :}
   w hdr U32+  h hdr U32+
   DEPTH$ hdr VEC:APPEND  colour hdr BYTE+  METHODS$ hdr VEC:APPEND
   s" IHDR" hdr VEC:BYTES v CHUNK
   hdr VEC:DELETE ;

: IDAT ( str ^VEC:vector<u8> -- )
   {: rows:str v:^ :}
   FRESH {: packed:^ :}
   rows packed ZLIB:DEFLATE-FRAMED
   s" IDAT" packed VEC:BYTES v CHUNK
   packed VEC:DELETE ;

\ A fresh vector holding a PNG of 8-bit samples whose rows each start with
\ filter byte 0.
: PNG ( n n n str -- ^VEC:vector<u8> )
   {: w:n h:n colour:n rows:str :}
   FRESH {: v:^ :}
   SIGNATURE$ v VEC:APPEND
   w h colour v IHDR
   rows v IDAT
   s" IEND" s" " v CHUNK
   v ;

: RGB-ROWS$ ( -- str )
   s\" \x00\xFF\x00\x00\x00\xFF\x00\x00\x00\x00\xFF\x0A\x14\x1E" ;
: RGB-PIXELS$ ( -- str )
   s\" \xFF\x00\x00\x00\xFF\x00\x00\x00\xFF\x0A\x14\x1E" ;
\ Grey 40 opaque, 80 transparent, C0 opaque.
: GREY-ROWS$ ( -- str ) s\" \x00\x40\xFF\x80\x00\xC0\xFF" ;
: GREY-PIXELS$ ( -- str ) s\" \x40\x40\x40\xFF\xFF\xFF\xC0\xC0\xC0" ;

\ Decodes the PNG into a vector that held other bytes, which it replaces.
: DECODES ( ^VEC:vector<u8> n n str -- )
   {: png:^ w:n h:n want:str :}
   FRESH {: out:^ :}
   s" stale" out VEC:APPEND
   png VEC:BYTES out PNG:DECODE-RGB
   h T=  w T=
   out VEC:BYTES want T$=
   out VEC:DELETE  png VEC:DELETE ;

: RGB ( -- )
   s" an RGB PNG decodes to its rows, top first" T-LABEL
   2 2 COLOUR-RGB RGB-ROWS$ PNG 2 2 RGB-PIXELS$ DECODES ;

: GREY-ALPHA ( -- )
   s" grey with alpha decodes to RGB on white" T-LABEL
   3 1 COLOUR-GREY-ALPHA GREY-ROWS$ PNG 3 1 GREY-PIXELS$ DECODES ;

\ The RGB PNG's first bytes, as many as the quotation makes of its length,
\ throw PNG:E-DATA and leave the vector empty.
: CUT ( [ n -- n ] -- )
   {: keep :}
   2 2 COLOUR-RGB RGB-ROWS$ PNG {: png:^ :}
   FRESH {: out:^ :}
   s" stale" out VEC:APPEND
   png VEC:SPAN  png VEC:COUNT keep execute  SPAN:TAKE SPAN:$ {: cut:str :}
   [: cut out PNG:DECODE-RGB drop drop ;] PNG:E-DATA TTHROWSQ
   out VEC:COUNT 0 T=
   out VEC:DELETE  png VEC:DELETE ;

\ 20 bytes end inside IHDR; 20 short of the end is inside IDAT, before IEND.
: TRUNCATED ( -- )
   s" bytes cut inside the header throw PNG:E-DATA" T-LABEL
   [: drop 20 ;] CUT
   s" bytes cut inside the pixel data throw PNG:E-DATA" T-LABEL
   [: 20 - ;] CUT ;

\ A million pixels square take 3 TB as RGB, past libpng's 4 GiB, though each
\ side is within libpng's limit of a million.
: TOO-LARGE ( -- )
   s" pixels past 4 GiB throw PNG:E-DATA" T-LABEL
   1000000 1000000 COLOUR-RGB RGB-ROWS$ PNG {: png:^ :}
   FRESH {: out:^ :}
   s" stale" out VEC:APPEND
   [: png VEC:BYTES out PNG:DECODE-RGB drop drop ;] PNG:E-DATA TTHROWSQ
   out VEC:COUNT 0 T=
   out VEC:DELETE  png VEC:DELETE ;

public
: RUN ( -- )
   T-RESET
   RGB GREY-ALPHA TRUNCATED TOO-LARGE
   T-REPORT ;
;package

PNG-TEST:RUN
