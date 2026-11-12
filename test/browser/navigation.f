\ navigation.f - Wasm fixture for the browser navigation, tree and highlight
\ protocol, built by `tools/hb-build.f -- --target wasm`, whose entry is MAIN.
require src/lib/string.f
require src/lib/browser/event.f

package NAV
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )

256 BUFFER: TEXT
variable TEXT-U
variable PRESS-BUTTONS
variable PRESS-MODS
68 BUFFER: GEOMETRY
128 BUFFER: INSTANCE-ROWS
4 BUFFER: WORDS
64 BUFFER: MATRIX

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c! TEXT-U @ 1+ TEXT-U ! ;

: SAY ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CHAR+ loop ;

: DIGITS ( n -- )
   {: v:n :}
   v 10 / {: q:n :}
   q 0<> if q RECURSE then
   v q 10 * - 48 + CHAR+ ;

: SIGNED ( n -- )
   dup 0< if 45 CHAR+ negate then DIGITS ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: TREE ( ptr u8 n n -- )
   {: a:ptr u:n key:n :}
   5 a BYTES>N u key RECORD ;

: MESH ( n n -- )
   {: u:n handle:n :}
   13 GEOMETRY BYTES>N u handle RECORD ;

: UNMESH ( n -- )
   {: handle:n :}
   14 handle 0 0 RECORD ;

: HIGHLIGHT ( n -- )
   {: count:n :}
   6 WORDS BYTES>N count 0 RECORD ;

: DRAW ( n -- )
   {: count:n :}
   2 INSTANCE-ROWS BYTES>N count MATRIX BYTES>N RECORD ;

: U16! ( n ptr u8 -- )
   {: bits:n dst:ptr :}
   2 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: U32! ( n ptr u8 -- )
   {: bits:n dst:ptr :}
   4 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: VERTEX ( n n n n -- )
   \ Vertex k at x, y and z 16384 on the grid, of a face.
   {: k:n x:n y:n face:n :}
   GEOMETRY 8 + k 8 * + {: at:ptr :}
   x at U16!  y at 2 + U16!  16384 at 4 + U16!  face at 6 + U16! ;

: INDEX! ( n n -- )
   \ Vertex v at triangle corner k, the vertices ending at byte 56.
   {: v:n k:n :}
   v GEOMETRY 56 + k 2 * + U16! ;

: ROW ( n n n n n -- )
   \ Instance row r: scale 2^-15 and x, y moved by tx (f32 bits) and -1.
   {: r:n tx:n id:n flags:n offset:n :}
   INSTANCE-ROWS r 64 * + {: at:ptr :}
   48 0 do 0 at i + c! loop
   $38000000 at U32!  $38000000 at 16 + U32!  $38000000 at 32 + U32!
   tx at 36 + U32!  $BF800000 at 40 + U32!
   7 at 48 + U32!  id at 52 + U32!  flags at 56 + U32!  offset at 60 + U32! ;

: INIT-GEOMETRY ( -- )
   \ Mesh 7: two separate front-facing triangles, faces 0 and 1. Instance 10
   \ selects face 0 through the highlight's one word, instance 11 every face.
   6 GEOMETRY U32!  2 GEOMETRY 4 + U32!
   0 0 0 0 VERTEX  1 16384 0 0 VERTEX  2 0 16384 0 VERTEX
   3 32768 0 1 VERTEX  4 49152 0 1 VERTEX  5 32768 16384 1 VERTEX
   6 0 do i i INDEX! loop
   1 WORDS U32!
   0 $BF800000 10 0 0 ROW  1 0 11 1 $FFFFFFFF ROW
   64 0 do 0 MATRIX i + c! loop
   $3F800000 MATRIX U32!
   $3F800000 MATRIX 20 + U32!
   $3F800000 MATRIX 40 + U32!
   $3F800000 MATRIX 60 + U32! ;

: ROWS ( -- )
   \ The tree's rows through Shelf's label; the caller closes Shelf's row and
   \ the array.
   SB-RESET
   s\" [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"}," SB-APPEND
   s\" {\"key\":2,\"parent\":1,\"label\":\"Part\"}," SB-APPEND
   s\" {\"key\":3,\"parent\":2,\"label\":\"Face\"}," SB-APPEND
   s\" {\"key\":4,\"parent\":0,\"label\":\"Shelf\"" SB-APPEND ;

: START ( -- )
   INIT-GEOMETRY
   0 EVENT BYTES>N 0 0 RECORD
   ROWS s\" ,\"more\":true}]" SB-APPEND SB$ 2 TREE
   68 7 MESH 1 HIGHLIGHT 2 DRAW ;

: OPENED ( -- )
   \ The tree again with Shelf's two children.
   ROWS
   s\" },{\"key\":5,\"parent\":4,\"label\":\"Board\"}," SB-APPEND
   s\" {\"key\":6,\"parent\":4,\"label\":\"Bracket\"}]" SB-APPEND
   SB$ 2 TREE ;

: EVENT-TEXT ( -- )
   0 TEXT-U !
   0 EVENT@ SIGNED 58 CHAR+
   1 EVENT@ SIGNED 44 CHAR+ 2 EVENT@ SIGNED
   64 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 5 EVENT@ SIGNED 44 CHAR+ 6 EVENT@ SIGNED
   0 EVENT@ 6 = if
      s"  pressed " SAY PRESS-BUTTONS @ SIGNED 44 CHAR+ PRESS-MODS @ SIGNED
   then
   TEXT-RECORD ;

: KEY-TEXT ( -- )
   0 TEXT-U !
   s" key " SAY
   1 EVENT@ >BYTES 2 EVENT@ SAY
   32 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 6 EVENT@ SIGNED
   TEXT-RECORD ;

: IS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   2 EVENT@ u <> if false exit then
   u 0 ?do 1 EVENT@ i + >BYTES c@ a i + c@ <> if false unloop exit then loop
   true ;

: INVALID ( -- )
   s" json" IS? if s" nope" 0 TREE exit then
   s" utf8" IS? if 255 TEXT c! TEXT 1 0 TREE exit then
   s" duplicate" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\"},{\"key\":1,\"parent\":0,\"label\":\"B\"}]" 0 TREE exit then
   s" parent" IS? if
      s\" [{\"key\":2,\"parent\":1,\"label\":\"Child\"}]" 0 TREE exit then
   s" more" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\",\"more\":1}]" 0 TREE
      exit
   then
   s" unsent" IS? if
      SB-RESET
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\",\"more\":true}," SB-APPEND
      s\" {\"key\":2,\"parent\":1,\"label\":\"B\"}]" SB-APPEND
      SB$ 0 TREE exit
   then
   s" selected" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\"}]" 2 TREE exit then
   s" length" IS? if 67 8 MESH exit then
   s" index" IS? if 6 5 INDEX! 68 8 MESH exit then
   s" unheld" IS? if 9 INSTANCE-ROWS 48 + U32! 2 DRAW exit then
   s" unmesh" IS? if 9 UNMESH exit then
   s" offset" IS? if
      5 32768 16384 32 VERTEX 68 8 MESH
      8 INSTANCE-ROWS 48 + U32! 2 DRAW exit then
   s" handle" IS? if 68 $100000000 MESH exit then
   s" held" IS? if 68 7 MESH exit then
   s" shown" IS? if 7 UNMESH exit then
   s" short" IS? if 0 HIGHLIGHT exit then
   s" remesh" IS? if 0 DRAW 7 UNMESH 68 7 MESH 2 DRAW exit then
   s" flags" IS? if 2 INSTANCE-ROWS 56 + U32! 2 DRAW exit then
   s" zero" IS? if 0 DRAW 0 HIGHLIGHT exit then
   s" redraw" IS? if 2 DRAW exit then
   s" tree" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" 3 TREE exit then ;

: DISPATCH ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 4 = if INVALID exit then
   kind 10 = if 2 EVENT@ 1 = if OPENED exit then then
   kind 8 = if KEY-TEXT exit then
   kind 2 = if 5 EVENT@ PRESS-BUTTONS ! 6 EVENT@ PRESS-MODS ! then
   EVENT-TEXT ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package

: MAIN ( -- ) NAV:TURN ;
