\ display.f - checked Wasm peer for SVG images and typed-input control. It
\ echoes a load, focus or highlight event as a text record: the kind's name,
\ a space and the event's bytes in lowercase hex. Typed "fetch" fetches the
\ page-relative path slow. Typed "more" sends a tree whose row Shelf has more;
\ opening Shelf sends the text opened and the tree again with its child and
\ grandchild. Typed "draw" sends mesh 1, a square of two triangles of faces 0
\ and 1; mesh 2, one triangle of face 34 with normals (0, 0.8, -0.6); mesh
\ 3, that triangle with normals +x; a highlight of two words, bit 1 of the
\ first and bit 2 of the second set; and a draw of rows of handles 1, 2, 1,
\ 1 and 3: the orange left square, at offset 0, shows face 1 highlighted and
\ face 0 not, the green triangle above it, z scaled by 0.2, at offset 0,
\ shows face 34 highlighted, the white right square selects every face, the
\ blue square above it none, and the green triangle below the left square,
\ its z scaled by 0, none. Typed "unmesh", after "draw", draws the
\ triangle's row alone, unmeshes mesh 1 and draws that row again. Typed
\ "metal", after "draw", draws a copy of its rows with the orange square
\ metallic. Typed "zero" draws 0 rows. Typed "occlude", after "draw", sends
\ occlude 7 of two boxes past z 0.5: one behind the left square and one below
\ every instance; its answer comes back as the text occluded, the bytes in
\ hex and the id.
\ Typed "edges" sends occlude 8: a box behind the left square from z -0.5,
\ past the near plane, and one from the square's own z 0.5. Typed "many"
\ sends occlude 9 of 4097 boxes, the last behind the left square, the others
\ below every instance; its text gives bytes 4095 and 4096 in hex, the count
\ of 1s and the id, since the echo holds 1024 bytes. Typed "pick", after
\ "draw", picks with ids 1 to 4 a pixel of the left square's face 1, of the
\ right square's face 0, of the background and one below the canvas, over the
\ flat triangle's plane. Typed "nearer", after
\ "draw", draws ids 6 and 7, mesh 1 at z 0.5 and 0.625, the farther last,
\ through twice the identity moved by x 0.4, y -0.2 and z 0.5, so w is 2,
\ and picks with id 5 a pixel of their face 1 that the identity leaves
\ empty. Each picked event comes back as the text picked and its a, b, c and
\ d in decimal.
\ Built by `tools/hb-build.f -- --target wasm`, whose entry is MAIN.
require src/lib/string.f
require src/lib/browser/event.f

package DISPLAY
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )

92 BUFFER: SQUARE-MESH
50 BUFFER: TRIANGLE-MESH
50 BUFFER: FLAT-MESH
504 BUFFER: INSTANCE-ROWS
360 BUFFER: METAL-ROWS
8 BUFFER: WORDS
64 BUFFER: MATRIX
64 BUFFER: SHIFTED
4097 24 * BUFFER: BOXES
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
   m 8 + k 12 * + {: at:ptr :}
   x at U16!  y at 2 + U16!  0 at 4 + U16!  face at 6 + U16! ;

: SQUARE ( -- )
   \ The unit square's lower-left triangle is face 0, its upper-right face 1;
   \ its normals are +z, octahedral 0, 0; the six indices end at byte 92.
   SQUARE-MESH {: m:ptr :}
   6 m U32!  2 m 4 + U32!
   m 0 0 0 0 VERTEX  m 1 1 0 0 VERTEX  m 2 0 1 0 VERTEX
   m 3 1 0 1 VERTEX  m 4 1 1 1 VERTEX  m 5 0 1 1 VERTEX
   6 0 do i m 80 + i 2 * + U16! loop ;

: TRIANGLE ( ptr u8 n n -- )
   \ The unit square's lower-left half as face 34, whose highlight bit is bit
   \ 2 of the second word; the indices end at byte 50.
   {: m:ptr ox:n oy:n :}
   3 m U32!  1 m 4 + U32!
   m 0 0 0 34 VERTEX  m 1 1 0 34 VERTEX  m 2 0 1 34 VERTEX
   3 0 do ox m 16 + i 12 * + U16!  oy m 18 + i 12 * + U16! loop
   3 0 do i m 44 + i 2 * + U16! loop ;

: PLACE ( n n n n -- )
   \ Row r draws mesh handle scaled by 0.8 and moved to x tx and y ty (f32
   \ bits) and z 0.5.
   {: r:n handle:n tx:n ty:n :}
   INSTANCE-ROWS r 72 * + {: at:ptr :}
   $3F4CCCCD at U32!  $3F4CCCCD at 16 + U32!  $3F4CCCCD at 32 + U32!
   tx at 36 + U32!  ty at 40 + U32!  $3F000000 at 44 + U32!
   handle at 48 + U32! ;

: DEPTH ( n n -- )
   \ Row r's z scale, f32 bits.
   {: r:n z:n :}
   z INSTANCE-ROWS r 72 * + 32 + U32! ;

: MARK ( n n n n -- )
   \ Row r's id, flags and highlight offset.
   {: r:n id:n flags:n offset:n :}
   INSTANCE-ROWS r 72 * + {: at:ptr :}
   id at 52 + U32!  flags at 56 + U32!  offset at 60 + U32! ;

: PAINT ( n n n n -- )
   \ Row r's colour $rrggbb, metallic and roughness.
   {: r:n rgb:n metal:n rough:n :}
   INSTANCE-ROWS r 72 * + {: at:ptr :}
   rgb 16 rshift at 64 + c!  rgb 8 rshift $FF and at 65 + c!
   rgb $FF and at 66 + c!  metal at 68 + U16!  rough at 70 + U16! ;

: IDENTITY ( -- )
   $3F800000 MATRIX U32!  $3F800000 MATRIX 20 + U32!
   $3F800000 MATRIX 40 + U32!  $3F800000 MATRIX 60 + U32! ;

: DRAW ( ptr u8 n -- )
   {: rows:ptr count:n :}
   2 rows BYTES>N count MATRIX BYTES>N RECORD ;

: INSTANCES ( -- )
   \ x -0.9, 0.1 and y -1.2, -0.4, 0.5 as f32 bits; z scales 0.2 and 0.
   0 1 $BF666666 $BECCCCCD PLACE  0 1 0 0 MARK
   1 2 $BF666666 $3F000000 PLACE  1 2 0 0 MARK
   2 1 $3DCCCCCD $BECCCCCD PLACE  2 3 1 $FFFFFFFF MARK
   3 1 $3DCCCCCD $3F000000 PLACE  3 4 0 $FFFFFFFF MARK
   4 3 $BF666666 $BF99999A PLACE  4 5 0 $FFFFFFFF MARK
   1 $3E4CCCCD DEPTH  4 0 DEPTH
   0 $FF7314 0 32768 PAINT  1 $30A040 65535 16384 PAINT
   2 $FFFFFF 0 0 PAINT  3 $2060C0 0 65535 PAINT
   4 $30A040 0 0 PAINT ;

: METAL ( -- )
   \ A copy: the host reads a draw's rows when the turn ends.
   INSTANCE-ROWS METAL-ROWS 360 BYTE-COPY
   65535 METAL-ROWS 68 + U16!  METAL-ROWS 5 DRAW ;

: SHOW ( -- )
   SQUARE  TRIANGLE-MESH 14043 32767 TRIANGLE
   FLAT-MESH 32767 0 TRIANGLE
   2 WORDS U32!  4 WORDS 4 + U32!  IDENTITY  INSTANCES
   13 SQUARE-MESH BYTES>N 92 1 RECORD
   13 TRIANGLE-MESH BYTES>N 50 2 RECORD
   13 FLAT-MESH BYTES>N 50 3 RECORD
   6 WORDS BYTES>N 2 0 RECORD
   INSTANCE-ROWS 5 DRAW ;

: BOX ( n n n n n n n -- )
   \ Box k's lowest x, y, z, then its highest, as f32 bits.
   {: k:n x0:n y0:n z0:n x1:n y1:n z1:n :}
   BOXES k 24 * + {: at:ptr :}
   x0 at U32!  y0 at 4 + U32!  z0 at 8 + U32!
   x1 at 12 + U32!  y1 at 16 + U32!  z1 at 20 + U32! ;

: HIDDEN ( n -- )
   \ Box k at x -0.7..-0.3, y -0.2..0.2, z 0.6..0.8.
   $BF333333 $BE4CCCCD $3F19999A $BE99999A $3E4CCCCD $3F4CCCCD BOX ;

: OPEN ( n -- )
   \ Box k at x -0.6..0.6, y -0.9..-0.6, z 0.6..0.8.
   $BF19999A $BF666666 $3F19999A $3F19999A $BF19999A $3F4CCCCD BOX ;

: OCCLUDE ( -- )
   0 HIDDEN 1 OPEN  16 BOXES BYTES>N 48 7 RECORD ;

: EDGES ( -- )
   \ x -0.7..-0.3, y -0.2..0.2; z -0.5..0.6 and z 0.5..0.8.
   0 $BF333333 $BE4CCCCD $BF000000 $BE99999A $3E4CCCCD $3F19999A BOX
   1 $BF333333 $BE4CCCCD $3F000000 $BE99999A $3E4CCCCD $3F4CCCCD BOX
   16 BOXES BYTES>N 48 8 RECORD ;

: MANY ( -- )
   4096 0 do i OPEN loop  4096 HIDDEN
   16 BOXES BYTES>N 4097 24 * 9 RECORD ;

: UNMESH ( -- )
   INSTANCE-ROWS 72 + 1 DRAW  14 1 0 0 RECORD  INSTANCE-ROWS 72 + 1 DRAW ;

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

: PICK ( n n n -- )
   \ The pixel at clip x and y in tenths, from the typed event's buffer width
   \ and height, picked with an id.
   {: x:n y:n id:n :}
   15  10 x + 3 EVENT@ * 20 /  10 y - 4 EVENT@ * 20 /  id RECORD ;

: PICKS ( -- )
   -3 2 1 PICK  4 -2 2 PICK  0 7 3 PICK  -7 -11 4 PICK ;

: SHIFT ( -- )
   \ Twice the identity, w included, then x 0.4, y -0.2 and z 0.5 added.
   $40000000 SHIFTED U32!  $40000000 SHIFTED 20 + U32!
   $40000000 SHIFTED 40 + U32!  $40000000 SHIFTED 60 + U32!
   $3ECCCCCD SHIFTED 48 + U32!  $BE4CCCCD SHIFTED 52 + U32!
   $3F000000 SHIFTED 56 + U32! ;

: NEARER ( -- )
   \ Rows 5 and 6 at x and y -0.4; NDC 0.5, 0.2 is world 0.3, 0.3.
   5 1 $BECCCCCD $BECCCCCD PLACE  5 6 0 $FFFFFFFF MARK
   6 1 $BECCCCCD $BECCCCCD PLACE  6 7 0 $FFFFFFFF MARK
   $3F200000 INSTANCE-ROWS 6 72 * + 44 + U32!  SHIFT
   2 INSTANCE-ROWS 360 + BYTES>N 2 SHIFTED BYTES>N RECORD
   5 2 5 PICK ;

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
   s" metal" IS? if METAL exit then
   s" occlude" IS? if OCCLUDE exit then
   s" edges" IS? if EDGES exit then
   s" many" IS? if MANY exit then
   s" pick" IS? if PICKS exit then
   s" nearer" IS? if NEARER exit then
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

: CHAR+ ( n -- )
   ECHO-U @ ECHO-CAP >= if -1 throw then
   ECHO-BUF ECHO-U @ + c!  1 ECHO-U +! ;

: HEX, ( n -- )
   dup 10 < if [char] 0 + else 10 - [char] a + then CHAR+ ;

: DIGITS ( n -- )
   {: v:n :}
   v 10 / {: q:n :}
   q 0<> if q RECURSE then
   v q 10 * - [char] 0 + CHAR+ ;

: BYTE, ( n -- )
   {: v:n :}
   v 4 rshift HEX,  v 15 and HEX, ;

: BYTE@ ( n -- n )
   \ Byte k of the event's bytes.
   1 EVENT@ + >BYTES c@ ;

: NAMED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ECHO-BUF u BYTE-COPY u ECHO-U ! ;

\ The name and the event's bytes in ECHO-BUF; refuses an echo past the
\ buffer.
: ECHOED ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 2 EVENT@ dup + + ECHO-CAP > if -1 throw then
   a u NAMED
   2 EVENT@ 0 ?do i BYTE@ BYTE, loop ;

: ECHO-RECORD ( -- )
   3 ECHO-BUF BYTES>N ECHO-U @ 0 RECORD ;

: ECHO ( ptr u8 n -- )
   ECHOED ECHO-RECORD ;

: ONES ( -- n )
   0  2 EVENT@ 0 ?do i BYTE@ + loop ;

: SUMMARY ( -- )
   s" occluded " NAMED  4095 BYTE@ BYTE,  4096 BYTE@ BYTE,
   32 CHAR+  ONES DIGITS ;

: OCCLUDED ( -- )
   4 EVENT@ 9 = if SUMMARY else s" occluded " ECHOED then
   32 CHAR+  4 EVENT@ DIGITS  ECHO-RECORD ;

: SIGNED ( n -- )
   dup 0< if [char] - CHAR+ negate then DIGITS ;

: PICKED ( -- )
   s" picked" NAMED
   5 1 do 32 CHAR+ i EVENT@ SIGNED loop
   ECHO-RECORD ;

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
   0 EVENT@ 13 = if s" highlight " ECHO exit then
   0 EVENT@ 18 = if PICKED exit then
   0 EVENT@ 19 = if OCCLUDED then ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package

: MAIN ( -- ) DISPLAY:TURN ;
