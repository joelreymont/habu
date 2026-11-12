\ display.f - checked Wasm peer for SVG images and typed-input control. It
\ echoes a load, focus or highlight event as a text record: the kind's name,
\ a space and the event's bytes in lowercase hex. Typed "fetch" fetches the
\ page-relative path slow. Typed "more" sends a tree whose row Shelf has more;
\ opening Shelf sends the text opened and the tree again with its child and
\ grandchild. Typed "draw" sends mesh 1, a square of two triangles of faces 0
\ and 1; mesh 2, one triangle of face 34; a highlight of two words, bit 1 of
\ the first and bit 2 of the second set; and a draw of rows of handles 1, 2
\ and 1: the left square, at offset 0, shows face 1 highlighted and face 0
\ not, the triangle above it, at offset 0, shows face 34 highlighted, and the
\ right square selects every face. Typed "unmesh", after "draw", draws the
\ triangle's row alone, unmeshes mesh 1 and draws that row again. Typed
\ "zero" draws 0 rows.
\ Built by `tools/hb-build.f -- --target wasm`, whose entry is MAIN.
require src/lib/string.f
require src/lib/browser/event.f

package DISPLAY
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )

68 BUFFER: SQUARE-MESH
38 BUFFER: TRIANGLE-MESH
192 BUFFER: INSTANCE-ROWS
8 BUFFER: WORDS
64 BUFFER: MATRIX
512 BUFFER: IMAGE
variable IMAGE-U
1024 constant ECHO-CAP
ECHO-CAP BUFFER: ECHO-BUF
variable ECHO-U

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: SVG ( ptr u8 n -- )
   {: a:ptr u:n :}
   8 a BYTES>N u 0 RECORD ;

: IMAGE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a IMAGE IMAGE-U @ + u BYTE-COPY
   u IMAGE-U +! ;

: RED ( -- )
   0 IMAGE-U !
   s" <svg xmlns='http://www.w3.org/2000/svg' width='400' height='100' viewBox='0 0 400 100' onload='parent.svgScript=1'>" IMAGE+
   s" <script>parent.svgScript=2</script><a href='/svg-link'><rect width='400' height='100' fill='#d62828'/></a>" IMAGE+
   s" <image href='/svg-external' width='10' height='10'/></svg>" IMAGE+
   IMAGE IMAGE-U @ SVG ;

: BLUE ( -- )
   s\" <svg xmlns='http://www.w3.org/2000/svg' width='100' height='400' viewBox='0 0 100 400'><rect width='100' height='400' fill='#245bd6'/></svg>" SVG ;

: U16! ( n ptr u8 -- )
   {: bits:n dst:ptr :}
   2 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: U32! ( n ptr u8 -- )
   {: bits:n dst:ptr :}
   4 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: VERTEX ( ptr u8 n n n n -- )
   \ Vertex k of the mesh at m, at x, y and z 0 on the grid, of a face.
   {: m:ptr k:n x:n y:n face:n :}
   m 8 + k 8 * + {: at:ptr :}
   x at U16!  y at 2 + U16!  0 at 4 + U16!  face at 6 + U16! ;

: SQUARE ( -- )
   \ The unit square's lower-left triangle is face 0, its upper-right face 1;
   \ the six indices end at byte 68.
   SQUARE-MESH {: m:ptr :}
   6 m U32!  2 m 4 + U32!
   m 0 0 0 0 VERTEX  m 1 1 0 0 VERTEX  m 2 0 1 0 VERTEX
   m 3 1 0 1 VERTEX  m 4 1 1 1 VERTEX  m 5 0 1 1 VERTEX
   6 0 do i m 56 + i 2 * + U16! loop ;

: TRIANGLE ( -- )
   \ The unit square's lower-left half as face 34, whose highlight bit is bit
   \ 2 of the second word; the three indices end at byte 38.
   TRIANGLE-MESH {: m:ptr :}
   3 m U32!  1 m 4 + U32!
   m 0 0 0 34 VERTEX  m 1 1 0 34 VERTEX  m 2 0 1 34 VERTEX
   3 0 do i m 32 + i 2 * + U16! loop ;

: PLACE ( n n n n -- )
   \ Row r draws mesh handle scaled by 0.8 and moved to x tx and y ty (f32
   \ bits) and z 0.5.
   {: r:n handle:n tx:n ty:n :}
   INSTANCE-ROWS r 64 * + {: at:ptr :}
   $3F4CCCCD at U32!  $3F4CCCCD at 16 + U32!
   tx at 36 + U32!  ty at 40 + U32!  $3F000000 at 44 + U32!
   handle at 48 + U32! ;

: MARK ( n n n n -- )
   \ Row r's id, flags and highlight offset.
   {: r:n id:n flags:n offset:n :}
   INSTANCE-ROWS r 64 * + {: at:ptr :}
   id at 52 + U32!  flags at 56 + U32!  offset at 60 + U32! ;

: IDENTITY ( -- )
   $3F800000 MATRIX U32!  $3F800000 MATRIX 20 + U32!
   $3F800000 MATRIX 40 + U32!  $3F800000 MATRIX 60 + U32! ;

: DRAW ( ptr u8 n -- )
   {: rows:ptr count:n :}
   2 rows BYTES>N count MATRIX BYTES>N RECORD ;

: SHOW ( -- )
   \ x -0.9, 0.1 and y -0.4, 0.5 as f32 bits.
   SQUARE TRIANGLE  2 WORDS U32!  4 WORDS 4 + U32!  IDENTITY
   0 1 $BF666666 $BECCCCCD PLACE  0 1 0 0 MARK
   1 2 $BF666666 $3F000000 PLACE  1 2 0 0 MARK
   2 1 $3DCCCCCD $BECCCCCD PLACE  2 3 1 $FFFFFFFF MARK
   13 SQUARE-MESH BYTES>N 68 1 RECORD
   13 TRIANGLE-MESH BYTES>N 38 2 RECORD
   6 WORDS BYTES>N 2 0 RECORD
   INSTANCE-ROWS 3 DRAW ;

: UNMESH ( -- )
   INSTANCE-ROWS 64 + 1 DRAW  14 1 0 0 RECORD  INSTANCE-ROWS 64 + 1 DRAW ;

: ROWS ( -- )
   \ Van and its child Shelf through Shelf's label; the caller closes Shelf's
   \ row and the array.
   SB-RESET
   s\" [{\"key\":1,\"parent\":0,\"label\":\"Van\"}," SB-APPEND
   s\" {\"key\":2,\"parent\":1,\"label\":\"Shelf\"" SB-APPEND ;

: TREE ( ptr u8 n -- )
   \ The rows closed by the string, sent with no selection.
   SB-APPEND SB$ {: t:ptr tu:n :} 5 t BYTES>N tu 0 RECORD ;

: OPENED ( -- )
   \ Shelf's child Bracket arrives with Bracket's child Bolt.
   s" opened" {: a:ptr u:n :} 3 a BYTES>N u 0 RECORD
   ROWS s\" },{\"key\":3,\"parent\":2,\"label\":\"Bracket\"}," SB-APPEND
   s\" {\"key\":4,\"parent\":3,\"label\":\"Bolt\"}]" TREE ;

: IS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   2 EVENT@ u <> if false exit then
   u 0 ?do 1 EVENT@ i + >BYTES c@ a i + c@ <> if false unloop exit then loop
   true ;

: COMMAND ( -- )
   s" red" IS? if RED exit then
   s" blue" IS? if BLUE exit then
   s" zero" IS? if INSTANCE-ROWS 0 DRAW exit then
   s" draw" IS? if SHOW exit then
   s" unmesh" IS? if UNMESH exit then
   s" broken" IS? if s" <svg>" SVG exit then
   s" on" IS? if 9 1 0 0 RECORD exit then
   s" off" IS? if 9 0 0 0 RECORD exit then
   s" text" IS? if s" still here" {: a:ptr u:n :} 3 a BYTES>N u 0 RECORD exit then
   s" tree" IS? if s" []" {: a:ptr u:n :} 5 a BYTES>N u 0 RECORD exit then
   s" more" IS? if ROWS s\" ,\"more\":true}]" TREE exit then
   s" fetch" IS? if s" slow" {: a:ptr u:n :} 1 a BYTES>N u 0 RECORD exit then
   s" empty" IS? if 8 0 0 0 RECORD exit then
   s" negative" IS? if 8 0 -1 0 RECORD exit then
   s" outside" IS? if 8 -1 1 0 RECORD exit then
   s" past" IS? if 8 $7FFFFFFFFFFFFFFF 1 0 RECORD exit then
   s" svg-reserved" IS? if 8 0 1 1 RECORD exit then
   s" flag" IS? if 9 2 0 0 RECORD exit then
   s" negative-flag" IS? if 9 -1 0 0 RECORD exit then
   s" input-b" IS? if 9 1 1 0 RECORD exit then
   s" input-c" IS? if 9 0 0 1 RECORD exit then ;

: HEX, ( n -- )
   dup 10 < if [char] 0 + else 10 - [char] a + then
   ECHO-BUF ECHO-U @ + c!  1 ECHO-U +! ;

\ The event's bytes after the name, as one text record; refuses an echo past
\ the buffer.
: ECHO ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 2 EVENT@ dup + + ECHO-CAP > if -1 throw then
   a ECHO-BUF u BYTE-COPY u ECHO-U !
   2 EVENT@ 0 ?do
      1 EVENT@ i + >BYTES c@ {: v:n :}  v 4 rshift HEX,  v 15 and HEX,
   loop
   3 ECHO-BUF BYTES>N ECHO-U @ 0 RECORD ;

: DISPATCH ( -- )
   0 EVENT@ 0 = if
      0 EVENT BYTES>N 0 0 RECORD
      9 0 0 0 RECORD
      RED exit
   then
   0 EVENT@ 4 = if COMMAND exit then
   0 EVENT@ 10 = if 1 EVENT@ 2 = if 2 EVENT@ 1 = if OPENED then then exit then
   0 EVENT@ 11 = if s" load " ECHO exit then
   0 EVENT@ 12 = if s" focus " ECHO exit then
   0 EVENT@ 13 = if s" highlight " ECHO then ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package

: MAIN ( -- ) DISPLAY:TURN ;
