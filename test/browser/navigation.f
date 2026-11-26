\ navigation.f - Wasm fixture for the browser navigation, tree, toolbar,
\ highlight, pick, occlude, labels and section protocol, built by
\ `tools/hb-build.f -- --target wasm`, whose entry is MAIN. Typed "pick" picks
\ pixel 5, 6 with id 9; a picked event's fields come back as text, as any
\ event's but the light's do; a set event comes back as "set <key> <text>
\ <c>,<d>". Typed "labels" sends two labels, A at 1, 2, -0.5 and Ø6 at 0,
\ 0.25, 0; "labels-span" sends their span short of its last byte,
\ "labels-infinite" them with A's x infinite and "labels-utf8" them with A's
\ text the byte FF. Typed "section" sends the section z >= 0.5 capped in
\ c08040, "section-none" none and "section-short" its first 12 bytes;
\ "section-zero" sends it with a zero normal, "section-nan" with d NaN and
\ "section-colour" with its colour's fourth byte 1.
require src/lib/string.f
require src/lib/browser/event.f
require test/browser/turn-revision.f

package NAV
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ^u8 )
CAST: BYTES>N ( ^u8 -- n )
CAST: >CELLS ( ^u8 -- ^n )

256 BUFFER: TEXT
variable TEXT-U
variable PRESS-BUTTONS
variable PRESS-MODS
\ Mesh 7: counts and origin, 0, 0, 0, in 20 bytes, six vertices in 72, six
\ indices in 12.
104 BUFFER: GEOMETRY
144 BUFFER: INSTANCE-ROWS
4 BUFFER: WORDS
64 BUFFER: MATRIX
\ Two boxes of six f32, left zero: host-cli reads only their count.
48 BUFFER: BOXES
\ A toolbar whose one icon is a byte past the 2048 a row may state.
2112 BUFFER: LONG
\ Two labels, of 17 and 19 bytes.
36 BUFFER: LABELS
20 BUFFER: SECTION

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c! TEXT-U @ 1+ TEXT-U ! ;

: SAY ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ CHAR+ loop ;

: DIGITS ( n -- )
   {: v:n :}
   v 10 / {: q:n :}
   q 0<> if q RECURSE then
   v q 10 * - 48 + CHAR+ ;

: SIGNED ( n -- )
   dup 0< if 45 CHAR+ negate then DIGITS ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: TREE ( str n -- )
   {: s:str key:n :}
   5 s STR:DATA BYTES>N s STR:LEN key RECORD ;

: TOOLBAR ( str -- )
   {: s:str :}
   18 s STR:DATA BYTES>N s STR:LEN 0 RECORD ;

: PUT ( str n -- n )
   \ s at LONG's offset at, answering the offset past it.
   {: s:str at:n :}
   s STR:LEN 0 ?do s STR:DATA i + c@ LONG at + i + c! loop
   at s STR:LEN + ;

: LONG-ICON ( -- )
   s\" [{\"key\":\"a\",\"label\":\"A\",\"icon\":\"" 0 PUT {: at:n :}
   2049 0 do 77 LONG at + i + c! loop
   s\" \"}]" at 2049 + PUT {: u:n :}
   LONG u >str TOOLBAR ;

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

: U16! ( n ^u8 -- )
   {: bits:n dst:^ :}
   2 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: U32! ( n ^u8 -- )
   {: bits:n dst:^ :}
   4 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: VERTEX ( n n n n -- )
   \ Vertex k at x, y and z 16384 on the grid, of a face; its normal, +z, is
   \ octahedral 0, 0, the zeroed bytes after it.
   {: k:n x:n y:n face:n :}
   GEOMETRY 20 + k 12 * + {: at:^ :}
   x at U16!  y at 2 + U16!  16384 at 4 + U16!  face at 6 + U16! ;

: SECTION! ( -- )
   \ The plane 0, 0, 1, -0.5 as f32 bits, then the cap's colour c08040 and 0.
   0 SECTION U32!  0 SECTION 4 + U32!  $3F800000 SECTION 8 + U32!
   $BF000000 SECTION 12 + U32!  $004080C0 SECTION 16 + U32! ;

: INDEX! ( n n -- )
   \ Vertex v at triangle corner k, the vertices ending at byte 92.
   {: v:n k:n :}
   v GEOMETRY 92 + k 2 * + U16! ;

: ROW ( n n n n n -- )
   \ Instance row r: scale 2^-15 and x, y moved by tx (f32 bits) and -1.
   {: r:n tx:n id:n flags:n offset:n :}
   INSTANCE-ROWS r 72 * + {: at:^ :}
   48 0 do 0 at i + c! loop
   $38000000 at U32!  $38000000 at 16 + U32!  $38000000 at 32 + U32!
   tx at 36 + U32!  $BF800000 at 40 + U32!
   7 at 48 + U32!  id at 52 + U32!  flags at 56 + U32!  offset at 60 + U32! ;

: PAINT ( n n n n -- )
   \ Row r's colour $rrggbb, metallic and roughness.
   {: r:n rgb:n metal:n rough:n :}
   INSTANCE-ROWS r 72 * + {: at:^ :}
   rgb 16 rshift at 64 + c!  rgb 8 rshift $FF and at 65 + c!
   rgb $FF and at 66 + c!  metal at 68 + U16!  rough at 70 + U16! ;

: INIT-GEOMETRY ( -- )
   \ Mesh 7: two separate front-facing triangles, faces 0 and 1. Instance 10
   \ selects face 0 through the highlight's one word, instance 11 every face.
   6 GEOMETRY U32!  2 GEOMETRY 4 + U32!
   0 0 0 0 VERTEX  1 16384 0 0 VERTEX  2 0 16384 0 VERTEX
   3 32768 0 1 VERTEX  4 49152 0 1 VERTEX  5 32768 16384 1 VERTEX
   6 0 do i i INDEX! loop
   1 WORDS U32!
   0 $BF800000 10 0 0 ROW  1 0 11 1 $FFFFFFFF ROW
   0 $FF7314 0 32768 PAINT  1 $2060C0 65535 0 PAINT
   64 0 do 0 MATRIX i + c! loop
   $3F800000 MATRIX U32!
   $3F800000 MATRIX 20 + U32!
   $3F800000 MATRIX 40 + U32!
   $3F800000 MATRIX 60 + U32! ;

: LABEL ( n n n n str -- n )
   \ The label at byte at, x, y and z as f32 bits, of text s; answers the
   \ byte after it.
   {: at:n x:n y:n z:n s:str :}
   LABELS at + {: p:^ :}
   x p U32!  y p 4 + U32!  z p 8 + U32!  s STR:LEN p 12 + U32!
   s STR:DATA p 16 + s STR:LEN BYTE-COPY
   at 16 + s STR:LEN + ;

: LABELS! ( -- )
   \ The two labels.
   0 $3F800000 $40000000 $BF000000 s" A" LABEL
   0 $3E800000 0 s" Ø6" LABEL drop ;

: LABELLED ( n -- )
   \ A labels record of the first n bytes of LABELS.
   {: n:n :}
   19 LABELS BYTES>N n 0 RECORD ;

: SECTIONED ( -- )
   \ A section record of SECTION's 20 bytes.
   20 SECTION BYTES>N 20 0 RECORD ;

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
   0 EVENT BYTES>N TURN-REVISION:PROTOCOL 0 RECORD
   ROWS s\" ,\"more\":true}]" SB-APPEND SB$ 2 TREE
   104 7 MESH 1 HIGHLIGHT 2 DRAW ;

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
   32 CHAR+ 7 EVENT@ SIGNED 44 CHAR+ 8 EVENT@ SIGNED
   0 EVENT@ 6 = if
      s"  pressed " SAY PRESS-BUTTONS @ SIGNED 44 CHAR+ PRESS-MODS @ SIGNED
   then
   TEXT-RECORD ;

: HEX+ ( n -- )
   dup 10 < if 48 + else 87 + then CHAR+ ;

: OCCLUDED-TEXT ( -- )
   \ The answer's bytes in hex, then c,d buttons,mods.
   0 TEXT-U !
   s" occluded " SAY
   2 EVENT@ 0 ?do
      1 EVENT@ i + >BYTES c@ {: v:n :}  v 4 rshift HEX+  v 15 and HEX+
   loop
   32 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 5 EVENT@ SIGNED 44 CHAR+ 6 EVENT@ SIGNED
   TEXT-RECORD ;

: KEY-TEXT ( -- )
   0 TEXT-U !
   s" key " SAY
   1 EVENT@ >BYTES 2 EVENT@ >str SAY
   32 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 6 EVENT@ SIGNED
   TEXT-RECORD ;

: SET-TEXT ( -- )
   \ The set's key and text after its u64 key length, then c,d.
   0 TEXT-U !
   1 EVENT@ {: a:n :}
   a >BYTES >CELLS @ {: u:n :}
   s" set " SAY
   a 8 + >BYTES u >str SAY 32 CHAR+
   a 8 + u + >BYTES 2 EVENT@ 8 - u - >str SAY
   32 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   TEXT-RECORD ;

: IS? ( str -- bool )
   {: s:str :}
   2 EVENT@ s STR:LEN <> if false exit then
   s STR:LEN 0 ?do 1 EVENT@ i + >BYTES c@ s STR:DATA i + c@ <> if false unloop exit then loop
   true ;

: INVALID ( -- )
   s" json" IS? if s" nope" 0 TREE exit then
   s" utf8" IS? if 255 TEXT c! TEXT 1 >str 0 TREE exit then
   s" duplicate" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\"},{\"key\":1,\"parent\":0,\"label\":\"B\"}]" 0 TREE exit then
   s" parent" IS? if
      s\" [{\"key\":2,\"parent\":1,\"label\":\"Child\"}]" 0 TREE exit then
   s" more" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\",\"more\":1}]" 0 TREE
      exit
   then
   s" dim" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\",\"dim\":1}]" 0 TREE
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
   s" length" IS? if 103 8 MESH exit then
   s" index" IS? if 6 5 INDEX! 104 8 MESH exit then
   s" unheld" IS? if 9 INSTANCE-ROWS 48 + U32! 2 DRAW exit then
   s" unmesh" IS? if 9 UNMESH exit then
   s" offset" IS? if
      5 32768 16384 32 VERTEX 104 8 MESH
      8 INSTANCE-ROWS 48 + U32! 2 DRAW exit then
   s" handle" IS? if 104 $100000000 MESH exit then
   s" held" IS? if 104 7 MESH exit then
   s" shown" IS? if 7 UNMESH exit then
   s" short" IS? if 0 HIGHLIGHT exit then
   s" remesh" IS? if 0 DRAW 7 UNMESH 104 7 MESH 2 DRAW exit then

   s" flags" IS? if 4 INSTANCE-ROWS 56 + U32! 2 DRAW exit then
   s" colour" IS? if 1 INSTANCE-ROWS 67 + c! 2 DRAW exit then
   s" zero" IS? if 0 DRAW 0 HIGHLIGHT exit then
   s" redraw" IS? if 2 DRAW exit then
   s" pick" IS? if 15 5 6 9 RECORD exit then
   s" pick-id" IS? if 15 5 6 0 RECORD exit then
   s" occlude" IS? if 16 BOXES BYTES>N 48 9 RECORD 2 DRAW exit then
   s" span" IS? if 16 BOXES BYTES>N 47 9 RECORD exit then
   s" id" IS? if 16 BOXES BYTES>N 48 0 RECORD exit then
   s" labels" IS? if LABELS! 36 LABELLED exit then
   s" labels-span" IS? if LABELS! 35 LABELLED exit then
   s" labels-infinite" IS? if LABELS! $7F800000 LABELS U32! 36 LABELLED exit then
   s" labels-utf8" IS? if LABELS! $FF LABELS 16 + c! 36 LABELLED exit then
   s" section" IS? if SECTION! SECTIONED exit then
   s" section-none" IS? if 20 0 0 0 RECORD exit then
   s" section-short" IS? if SECTION! 20 SECTION BYTES>N 12 0 RECORD exit then
   s" section-zero" IS? if SECTION! 0 SECTION 8 + U32! SECTIONED exit then
   s" section-nan" IS? if SECTION! $7FC00000 SECTION 12 + U32! SECTIONED exit then
   s" section-colour" IS? if SECTION! $014080C0 SECTION 16 + U32! SECTIONED exit then
   s" tree" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\",\"dim\":true}]" 3 TREE exit then ;

: BAR ( -- )
   \ A toolbar of every kind, a button with an icon and a shortcut; then one
   \ toolbar per rule a row breaks.
   s" toolbar" IS? if
      SB-RESET
      s\" [{\"key\":\"fit\",\"label\":\"Fit\",\"shortcut\":\"f\"," SB-APPEND
      s\" \"icon\":\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\"}," SB-APPEND
      s\" {\"key\":\"mark\",\"label\":\"Mark\",\"pressed\":true}," SB-APPEND
      s\" {\"key\":\"top\",\"label\":\"Top\",\"kind\":\"item\",\"group\":\"Views\",\"pressed\":false}," SB-APPEND
      s\" {\"key\":\"cut\",\"label\":\"Section\",\"kind\":\"range\",\"min\":-50,\"max\":50,\"value\":0}," SB-APPEND
      s\" {\"key\":\"find\",\"label\":\"Filter\",\"kind\":\"text\",\"value\":\"\"}]" SB-APPEND
      SB$ TOOLBAR exit
   then
   s" bar-key" IS? if s\" [{\"key\":\"\",\"label\":\"A\"}]" TOOLBAR exit then
   s" bar-twice" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\"},{\"key\":\"a\",\"label\":\"B\"}]" TOOLBAR exit then
   s" bar-label" IS? if s\" [{\"key\":\"a\",\"label\":1}]" TOOLBAR exit then
   s" bar-label-empty" IS? if s\" [{\"key\":\"a\",\"label\":\"\"}]" TOOLBAR exit then
   s" bar-kind" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"toggle\"}]" TOOLBAR exit then
   s" bar-member" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"step\":1}]" TOOLBAR exit then
   s" bar-group" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"group\":1}]" TOOLBAR exit then
   s" bar-group-empty" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"group\":\"\"}]" TOOLBAR exit then
   s" bar-item" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"item\"}]" TOOLBAR exit then
   s" bar-pressed" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"pressed\":1}]" TOOLBAR exit then
   s" bar-pressed-range" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"range\",\"min\":0,\"max\":1,\"value\":0,\"pressed\":true}]" TOOLBAR exit then
   s" bar-integer" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"range\",\"min\":0.5,\"max\":1,\"value\":1}]" TOOLBAR exit then
   s" bar-order" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"range\",\"min\":5,\"max\":5,\"value\":5}]" TOOLBAR exit then
   s" bar-within" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"range\",\"min\":0,\"max\":10,\"value\":11}]" TOOLBAR exit then
   s" bar-value" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"text\",\"value\":1}]" TOOLBAR exit then
   s" bar-icon" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"icon\":\"\"}]" TOOLBAR exit then
   s" bar-long" IS? if LONG-ICON exit then
   s" bar-markup" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"icon\":\"M0 0<path/>\"}]" TOOLBAR exit then
   s" bar-shortcut-twice" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"shortcut\":\"h\"},{\"key\":\"b\",\"label\":\"B\",\"shortcut\":\"h\"}]" TOOLBAR exit then
   s" bar-shortcut-help" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"shortcut\":\"?\"}]" TOOLBAR exit then
   s" bar-shortcut-escape" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"shortcut\":\"Escape\"}]" TOOLBAR exit then
   s" bar-shortcut-tab" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"shortcut\":\"Tab\"}]" TOOLBAR exit then
   s" bar-shortcut-key" IS? if
      s\" [{\"key\":\"g\",\"label\":\"G\"},{\"key\":\"group\",\"label\":\"Group\",\"shortcut\":\"g\"}]" TOOLBAR exit then
   s" bar-shortcut-range" IS? if
      s\" [{\"key\":\"a\",\"label\":\"A\",\"kind\":\"range\",\"min\":0,\"max\":1,\"value\":0,\"shortcut\":\"r\"}]" TOOLBAR exit then ;

: DISPATCH ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 4 = if INVALID BAR exit then
   kind 10 = if 2 EVENT@ 1 = if OPENED exit then then
   kind 8 = if KEY-TEXT exit then
   kind 19 = if OCCLUDED-TEXT exit then
   kind 21 = if SET-TEXT exit then
   kind 20 = if exit then
   kind 2 = if 5 EVENT@ PRESS-BUTTONS ! 6 EVENT@ PRESS-MODS ! then
   EVENT-TEXT ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package

: MAIN ( -- ) NAV:TURN ;
