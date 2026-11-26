\ png.f - a PNG decoded into 8-bit sRGB RGB rows by libpng16's simplified read
\ API. libpng converts every colour type and depth to three bytes a pixel
\ and composites any alpha onto white, the colour of a page. A failure is
\ logged, with libpng's message where libpng rejected the bytes, then thrown
\ as E-DATA.
\ STORAGE CLASS. SCOPE-OWNED: a call's png_image is a MEM:WITH-RECORD record,
\ and libpng's state in it is freed when the call returns or throws.

require src/lib/error.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/log.f

package PNG

private
-10100 constant E-FIRST
-10109 constant E-LAST

public
-10100 s" PNG bytes libpng cannot read, or pixels past its 4 GiB limit; the log line says which"
   ERROR:CODE E-DATA

private

\ png_image on the supported LP64 hosts: the control pointer, seven
\ png_uint_32 and a 64-byte message, 100 bytes padded to 104.
STRUCTURE image OPAQUE
   BUFFER bytes 104
;STRUCTURE

8 constant VERSION-AT
12 constant WIDTH-AT
16 constant HEIGHT-AT
20 constant FORMAT-AT
36 constant MESSAGE-AT
1 constant IMAGE-VERSION
\ PNG_FORMAT_RGB: colour, no alpha, 8-bit sRGB.
2 constant FORMAT-RGB
3 constant RGB-BYTES
\ libpng holds a decoded image's bytes in 32 bits.
$FFFFFFFF constant U32-MAX

VERSIONED-LIBRARY png16 16
FUNCTION: C-BEGIN png_image_begin_read_from_memory
   ( ^u8 str -- i32 ) ;FUNCTION
FUNCTION: C-FINISH png_image_finish_read
   ( ^u8 ^u8 ^u8 i32 ^u8 -- i32 ) ;FUNCTION
FUNCTION: C-FREE png_image_free ( ^u8 -- ) ;FUNCTION

\ png_color, red, green and blue.
: WHITE ( -- ^u8 ) s\" \xFF\xFF\xFF" STR:DATA ;
: NULL-ARG ( -- ^u8 ) NULL-PTR BYTE-VIEW ;

: U32-AT ( ^u8 n -- n ) + LE:U32@ ;

: LOG-OPEN ( -- ) LOG:ERROR s" png" LOG:LINE ;

\ libpng ends its message with a NUL inside the 64 bytes.
: FAIL ( ^u8 -- )
   MESSAGE-AT + dup ZLEN >str {: msg:str :}
   LOG-OPEN msg LOG:TEXT+ LOG:;LINE
   E-DATA throw ;

: TOO-LARGE ( n n -- )
   {: w:n h:n :}
   LOG-OPEN w LOG:INT+ s"  by " LOG:TEXT+ h LOG:INT+
   s"  pixels take more than 4 GiB as RGB" LOG:TEXT+ LOG:;LINE
   E-DATA throw ;

\ The size check is libpng's own, made before the vector grows. Stride 0
\ packs the rows, top row first.
: FINISH ( ^u8 ^VEC:vector<u8> -- n n )
   {: p:^ v:^ :}
   FORMAT-RGB p FORMAT-AT + LE:U32!
   p WIDTH-AT U32-AT {: w:n :}
   p HEIGHT-AT U32-AT {: h:n :}
   h U32-MAX w RGB-BYTES * / > if w h TOO-LARGE then
   v w h * RGB-BYTES * VEC:RESIZE
   p WHITE v VEC:BYTES STR:DATA 0 NULL-ARG C-FINISH
   0= if p FAIL then
   w h ;

\ png_image_free is a no-op on an image libpng has freed already.
: READ ( ^image str ^VEC:vector<u8> -- n n )
   {: img:^ src:str v:^ :}
   img BYTE-VIEW {: p:^ :}
   IMAGE-VERSION p VERSION-AT + LE:U32!
   p src C-BEGIN 0= if p FAIL then
   [: p v FINISH ;] [: p C-FREE ;] finally ;

: DECODE ( str ^VEC:vector<u8> -- n n )
   {: src:str v:^ :}
   [type] image [: src v READ ;] MEM:WITH-RECORD ;

public

\ Replaces the vector's bytes with the PNG's pixels as 8-bit RGB rows, top row
\ first, no padding, and answers its width and height. The source must not
\ lie in the vector's block. On a throw the vector is empty.
: DECODE-RGB ( str ^VEC:vector<u8> -- n n )
   {: src:str v:^ :}
   [: src v DECODE ;] catch {: rc:n :}
   rc 0<> if v 0 VEC:RESIZE rc throw then ;

;package
