\ font.f - one selected font face and one LTR SVG glyph run.
require src/lib/ffi-abi.f
require src/lib/utf8-scalar.f
require src/lib/float32.f
require src/lib/ieee754.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/image-lifecycle.f
require src/lib/task.f
require src/lib/string.f
require src/lib/f64-text.f

package FONT
public
NEWTYPE font
NEWTYPE image

$7B00 constant E-OPEN
$7B01 constant E-HANDLE
$7B02 constant E-TEXT
$7B03 constant E-GLYPH
$7B04 constant E-RENDER

private
CAST: >FONT ( n -- font )
CAST: FONT>N ( font -- n )
CAST: >IMAGE ( n -- image )
CAST: IMAGE>N ( image -- n )

$73766720 constant SVG-FORMAT
20 constant GLYPH-INFO-BYTES
20 constant GLYPH-POS-BYTES
16 constant EXTENTS-BYTES
8 constant NODE-CELLS
$7FFFFFFF constant MAX-TEXT
$FFFFFFFF constant MAX-FONT
\ The final viewBox goes through HarfBuzz's binary32 extents.
$1000000 constant MAX-EXACT-F32
$5A797979 constant SCRIPT-COMMON
$5A696E68 constant SCRIPT-INHERITED
$5A7A7A7A constant SCRIPT-UNKNOWN

VERSIONED-LIBRARY harfbuzz 0
FUNCTION: BLOB-CREATE hb_blob_create_or_fail
   ( ptr u8 n n ptr u8 ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: BLOB-DESTROY hb_blob_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: BLOB-LEN hb_blob_get_length ( ptr u8 -- u32 ) ;FUNCTION
FUNCTION: BLOB-DATA hb_blob_get_data ( ptr u8 ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: FACE-CREATE hb_face_create_or_fail ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: FACE-REF hb_face_reference ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: FACE-DESTROY hb_face_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: FONT-CREATE hb_font_create ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: FONT-DESTROY hb_font_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: FONT-SCALE hb_font_set_scale ( ptr u8 n n -- ) ;FUNCTION
FUNCTION: BUF-CREATE hb_buffer_create ( -- ptr u8 ) ;FUNCTION
FUNCTION: BUF-DESTROY hb_buffer_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: BUF-ADD hb_buffer_add_utf8
   ( ptr u8 ptr u8 n n n -- ) ;FUNCTION
FUNCTION: BUF-GUESS hb_buffer_guess_segment_properties ( ptr u8 -- ) ;FUNCTION
FUNCTION: BUF-DIRECTION hb_buffer_get_direction ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: BUF-LEN hb_buffer_get_length ( ptr u8 -- u32 ) ;FUNCTION
FUNCTION: BUF-INFO hb_buffer_get_glyph_infos ( ptr u8 ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: BUF-POS hb_buffer_get_glyph_positions ( ptr u8 ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: SHAPE hb_shape ( ptr u8 ptr u8 ptr u8 n -- ) ;FUNCTION
FUNCTION: UNICODE-DEFAULT hb_unicode_funcs_get_default ( -- ptr u8 ) ;FUNCTION
FUNCTION: UNICODE-CATEGORY hb_unicode_general_category ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: UNICODE-SCRIPT hb_unicode_script ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: SCRIPT-DIRECTION hb_script_get_horizontal_direction ( n -- i32 ) ;FUNCTION
FUNCTION: COLOR-LAYERS hb_ot_color_glyph_get_layers
   ( ptr u8 n n ptr u8 ptr u8 -- u32 ) ;FUNCTION
FUNCTION: COLOR-PAINT hb_ot_color_glyph_has_paint
   ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: COLOR-SVG hb_ot_color_glyph_reference_svg
   ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: COLOR-PNG hb_ot_color_glyph_reference_png
   ( ptr u8 n -- ptr u8 ) ;FUNCTION

VERSIONED-LIBRARY harfbuzz-vector 0
FUNCTION: DRAW-CREATE hb_vector_draw_create_or_fail ( n -- ptr u8 ) ;FUNCTION
FUNCTION: DRAW-DESTROY hb_vector_draw_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: DRAW-TRANSFORM hb_vector_draw_set_transform
   ( ptr u8 f32 f32 f32 f32 f32 f32 -- ) ;FUNCTION
FUNCTION: DRAW-GLYPH hb_vector_draw_glyph_or_fail
   ( ptr u8 ptr u8 n n -- i32 ) ;FUNCTION
FUNCTION: DRAW-PRECISION hb_vector_draw_set_precision ( ptr u8 n -- ) ;FUNCTION
FUNCTION: DRAW-EXTENTS hb_vector_draw_get_extents ( ptr u8 ptr u8 -- i32 )
   1 EXTENTS-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: DRAW-SET-EXTENTS hb_vector_draw_set_extents ( ptr u8 ptr u8 -- ) ;FUNCTION
FUNCTION: DRAW-RENDER hb_vector_draw_render ( ptr u8 -- ptr u8 ) ;FUNCTION

PTR-VARIABLE FONTS
PTR-VARIABLE IMAGES
variable SERIAL
variable REGISTERED
here data-base - negate 7 and allot
variable LOCK-CELL

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 5 cells TASK:+USER RENDER-STATE drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CELL TASK:+USER OPEN-STATE drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 7 cells TASK:+USER HULL-STATE drop
: REF-CELL ( -- ptr ptr u8 ) RENDER-STATE 0 ptr-field ;
: FONT-CELL ( -- ptr ptr u8 ) RENDER-STATE 1 ptr-field ;
: BUF-CELL ( -- ptr ptr u8 ) RENDER-STATE 2 ptr-field ;
: DRAW-CELL ( -- ptr ptr u8 ) RENDER-STATE 3 ptr-field ;
: BLOB-CELL ( -- ptr ptr u8 ) RENDER-STATE 4 ptr-field ;
: OPEN-CELL ( -- ptr ptr u8 ) OPEN-STATE 0 ptr-field ;
: H-MINX ( -- ptr n ) HULL-STATE BYTE-VIEW CELL-VIEW ;
: H-MINY ( -- ptr n ) HULL-STATE BYTE-VIEW 1 cells + CELL-VIEW ;
: H-MAXX ( -- ptr n ) HULL-STATE BYTE-VIEW 2 cells + CELL-VIEW ;
: H-MAXY ( -- ptr n ) HULL-STATE BYTE-VIEW 3 cells + CELL-VIEW ;
: H-X ( -- ptr n ) HULL-STATE BYTE-VIEW 4 cells + CELL-VIEW ;
: H-PAIR ( -- ptr n ) HULL-STATE BYTE-VIEW 5 cells + CELL-VIEW ;
: H-SEEN ( -- ptr n ) HULL-STATE BYTE-VIEW 6 cells + CELL-VIEW ;

: LOCK ( -- )
   begin 0 1 LOCK-CELL atomic-cas 0= until ;
: UNLOCK ( -- ) 0 LOCK-CELL atomic! ;
: NULL? ( ptr a -- bool ) NULL-PTR = ;
: LINK ( ptr n -- ptr ptr n ) 0 ptr-field ;
: IDENT ( ptr n -- ptr n ) 1 cells + ;
: OBJECT ( ptr n -- ptr ptr u8 ) 2 ptr-field ;
: BX ( ptr n -- ptr n ) 3 cells + ;
: BY ( ptr n -- ptr n ) 4 cells + ;
: BW ( ptr n -- ptr n ) 5 cells + ;
: BH ( ptr n -- ptr n ) 6 cells + ;

: NEW-NODE ( ptr u8 -- ptr n ) {: object:ptr :}
   NODE-CELLS MEM-ALLOC-CELLS {: node:ptr :}
   NULL-PTR node LINK ! object node OBJECT ! node ;
: DROP-NODE ( ptr n -- )
   BYTE-VIEW NODE-CELLS cells MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;
: NEXT-ID ( -- n )
   SERIAL @ 1+ dup 0 <= if E-HANDLE throw then
   dup SERIAL ! ;
: FIND ( n ptr n -- ptr n ) {: wanted:n node:ptr :}
   node begin dup NULL? 0= while
      dup IDENT @ wanted = if exit then
      LINK @
   repeat ;
: REMOVE ( ptr n ptr ptr n -- ) {: node:ptr head:ptr :}
   head begin dup @ node <> while @ LINK repeat
   node LINK @ swap ! ;
: FONT-NODE ( font -- ptr n )
   FONT>N FONTS @ FIND dup NULL? if E-HANDLE throw then ;
: IMAGE-NODE ( image -- ptr n )
   IMAGE>N IMAGES @ FIND dup NULL? if E-HANDLE throw then ;

: FREE-FONTS ( -- )
   begin FONTS @ dup NULL? 0= while
      dup LINK @ FONTS !
      dup OBJECT @ FACE-DESTROY DROP-NODE
   repeat drop ;
: FREE-IMAGES ( -- )
   begin IMAGES @ dup NULL? 0= while
      dup LINK @ IMAGES !
      dup OBJECT @ BLOB-DESTROY DROP-NODE
   repeat drop ;
: CLEANUP ( -- )
   LOCK FREE-IMAGES FREE-FONTS 0 REGISTERED ! UNLOCK ;
: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: CLEANUP ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;
: KEEP-FONT ( ptr u8 -- font )
   {: face:ptr :}
   LOCK
   [: REGISTER-CLEANUP ;] [: UNLOCK ;] finally
   LOCK [: NEXT-ID ;] [: UNLOCK ;] finally {: ident:n :}
   face NEW-NODE {: node:ptr :}
   ident node IDENT !
   LOCK
   FONTS @ node LINK ! node FONTS !
   UNLOCK ident >FONT ;
: KEEP-IMAGE ( ptr u8 r r r r -- image )
   {: blob:ptr x:r y:r w:r h:r :}
   LOCK
   [: REGISTER-CLEANUP ;] [: UNLOCK ;] finally
   LOCK [: NEXT-ID ;] [: UNLOCK ;] finally {: ident:n :}
   blob NEW-NODE {: node:ptr :}
   x IEEE754:F64>BITS node BX !
   y IEEE754:F64>BITS node BY !
   w IEEE754:F64>BITS node BW !
   h IEEE754:F64>BITS node BH !
   ident node IDENT !
   LOCK
   IMAGES @ node LINK ! node IMAGES !
   UNLOCK ident >IMAGE ;

: OPEN-BLOB ( ptr u8 n n -- ptr u8 )
   {: data:ptr size:n index:n :}
   data size 0 NULL-PTR NULL-PTR BLOB-CREATE {: blob:ptr :}
   blob NULL? if E-OPEN throw then
   blob index FACE-CREATE {: face:ptr :}
   blob BLOB-DESTROY
   face NULL? if E-OPEN throw then
   face ;

: FONT-REF ( font -- ptr u8 )
   LOCK [: FONT-NODE OBJECT @ FACE-REF ;] [: UNLOCK ;] finally ;

: KEEP-OPEN ( -- font )
   OPEN-CELL @ KEEP-FONT
   NULL-PTR OPEN-CELL ! ;
: RELEASE-OPEN ( -- )
   OPEN-CELL @ dup NULL? 0= if FACE-DESTROY else drop then
   NULL-PTR OPEN-CELL ! ;

: EM-CHECK ( n -- n )
   dup 0 <= if E-TEXT throw then
   dup $1FFFFFF > if E-TEXT throw then ;

: SCRIPT-CHECK ( n n n -- n n )
   {: selected:n ink:n scalar:n :}
   UNICODE-DEFAULT scalar UNICODE-CATEGORY {: category:n :}
   category 0= category 1 = or if E-TEXT throw then
   scalar $2028 = scalar $2029 = or if E-TEXT throw then
   ink 0<> category 29 <> or if 1 else 0 then {: visible:n :}
   UNICODE-DEFAULT scalar UNICODE-SCRIPT {: script:n :}
   script SCRIPT-COMMON = script SCRIPT-INHERITED = or if
      selected visible exit
   then
   script SCRIPT-UNKNOWN = if E-TEXT throw then
   script SCRIPT-DIRECTION 4 <> if E-TEXT throw then
   selected 0= if script visible exit then
   selected script <> if E-TEXT throw then
   selected visible ;

: TEXT-STEP ( n n ptr u8 n n -- n n n )
   {: script:n ink:n data:ptr size:n cursor:n :}
   data size cursor UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF {: codepoint:n next:n :}
         script ink codepoint SCRIPT-CHECK next
      ENDOF
      raw-byte OF 2drop E-TEXT throw ENDOF
   ;MATCH ;

: STEP-IN ( n n n ptr u8 n -- n n n )
   {: script:n ink:n cursor:n data:ptr size:n :}
   script ink data size cursor TEXT-STEP ;

: TEXT-CHECK ( ptr u8 n -- n ) {: data:ptr size:n :}
   size 0 < size MAX-TEXT > or if E-TEXT throw then
   0 0 0 begin dup size < while data size STEP-IN repeat drop nip ;

: BLOB-PRESENT? ( ptr u8 -- bool )
   dup BLOB-LEN 0<> swap BLOB-DESTROY ;

: COLOR-GLYPH? ( ptr u8 n -- bool ) {: font:ptr glyph:n :}
   REF-CELL @ glyph 0 NULL-PTR NULL-PTR COLOR-LAYERS 0<>
   REF-CELL @ glyph COLOR-PAINT 0<> or
   REF-CELL @ glyph COLOR-SVG BLOB-PRESENT? or
   font glyph COLOR-PNG BLOB-PRESENT? or ;

\ HarfBuzz's glyph ink extents may be narrower than the decimal SVG path.
\ Its SVG draw backend emits absolute M/L/Q/C points under scale(1,-1).
: HULL-RESET ( -- ) 0 H-PAIR ! 0 H-SEEN ! ;
: HULL-FIRST ( r r -- ) {: x:r y:r :}
   x IEEE754:F64>BITS dup H-MINX ! H-MAXX !
   y IEEE754:F64>BITS dup H-MINY ! H-MAXY !
   1 H-SEEN ! ;
: HULL-ADD ( r r -- ) {: x:r y:r :}
   H-SEEN @ 0= if x y HULL-FIRST exit then
   x H-MINX @ IEEE754:BITS>F64 f< if x IEEE754:F64>BITS H-MINX ! then
   x H-MAXX @ IEEE754:BITS>F64 f> if x IEEE754:F64>BITS H-MAXX ! then
   y H-MINY @ IEEE754:BITS>F64 f< if y IEEE754:F64>BITS H-MINY ! then
   y H-MAXY @ IEEE754:BITS>F64 f> if y IEEE754:F64>BITS H-MAXY ! then ;
: HULL-VALUE ( r -- ) {: value:r :}
   H-PAIR @ 0= if
      value IEEE754:F64>BITS H-X ! 1 H-PAIR ! exit
   then
   H-X @ IEEE754:BITS>F64 value HULL-ADD
   0 H-PAIR ! ;
: PATH-SKIP? ( n -- bool ) {: char:n :}
   char $4D = char $4C = or char $51 = or char $43 = or
   char $5A = or char $2C = or char $20 = or ;
: PATH-DELIM? ( n -- bool )
   dup PATH-SKIP? swap $22 = or ;
: TOKEN-END ( ptr u8 n n -- n )
   {: svg:ptr size:n start:n :}
   start begin dup size < while
      svg over + c@ PATH-DELIM? if exit then
      1+
   repeat E-RENDER throw ;
: PATH-NUMBER ( ptr u8 n -- r )
   F64-TEXT:PARSE MATCH result
      ok OF ENDOF
      err OF drop E-RENDER throw ENDOF
   ;MATCH ;
: SCAN-NUMBER ( ptr u8 n n -- n )
   {: svg:ptr size:n start:n :}
   svg size start TOKEN-END {: stop:n :}
   stop start = if E-RENDER throw then
   svg start + stop start - PATH-NUMBER HULL-VALUE
   stop ;
: SCAN-PATH ( ptr u8 n n -- n )
   {: svg:ptr size:n start:n :}
   start begin dup size < while
      svg over + c@ dup $22 = if
         drop H-PAIR @ 0<> if E-RENDER throw then
         1+ exit
      then
      dup PATH-SKIP? if drop 1+ else
         drop svg size rot SCAN-NUMBER
      then
   repeat E-RENDER throw ;
: FIND-PATH-AT ( ptr u8 n n -- n )
   {: svg:ptr size:n start:n :}
   svg start + size start - s" <path d=" FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N start + ENDOF
   ;MATCH ;
: SCAN-SVG ( ptr u8 n -- ) {: svg:ptr size:n :}
   HULL-RESET
   0 begin dup size < while
      dup svg size rot FIND-PATH-AT dup 0< if 2drop exit then
      nip
      dup 8 + size >= if E-RENDER throw then
      svg over 8 + + c@ $22 <> if E-RENDER throw then
      9 + svg size rot SCAN-PATH
   repeat drop ;

: GLYPH-STEP ( ptr u8 ptr u8 ptr u8 ptr u8 n n -- n n )
   {: font:ptr draw:ptr info:ptr pos:ptr x:n y:n :}
   info LE:U32@ {: glyph:n :}
   glyph 0= if E-GLYPH throw then
   font glyph COLOR-GLYPH? if E-GLYPH throw then
   draw 0.015625 0.0 0.0 0.015625
      x pos 8 + LE:S32@ + s>f 64.0 f/
      y pos 12 + LE:S32@ + s>f 64.0 f/
      DRAW-TRANSFORM
   draw font glyph 1 DRAW-GLYPH 0= if E-GLYPH throw then
   x pos LE:S32@ + y pos 4 + LE:S32@ + ;

: STEP-AT ( n n ptr u8 ptr u8 ptr u8 ptr u8 -- n n )
   {: x:n y:n font:ptr draw:ptr info:ptr pos:ptr :}
   font draw info pos x y GLYPH-STEP ;

: DRAW-ALL ( ptr u8 ptr u8 ptr u8 -- )
   {: font:ptr buf:ptr draw:ptr :}
   buf BUF-LEN {: count:n :}
   buf NULL-PTR BUF-INFO {: info:ptr :}
   buf NULL-PTR BUF-POS {: pos:ptr :}
   0 0 count 0 ?do
      font draw info i GLYPH-INFO-BYTES * +
      pos i GLYPH-POS-BYTES * + STEP-AT
   loop 2drop ;

: EDGE-LOW ( r -- n )
   {: value:r :}
   value f>s {: whole:n :}
   value whole s>f f< if whole 1- else whole then ;
: EDGE-HIGH ( r -- n )
   {: value:r :}
   value f>s {: whole:n :}
   value whole s>f f> if whole 1+ else whole then ;
: F32@ ( ptr u8 -- r ) LE:U32@ F32:WIDEN ;
: F32! ( r ptr u8 -- ) swap F32:NARROW swap LE:U32! ;
: EMPTY-SVG$ ( -- ptr u8 n )
   s" <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 0 0' width='0' height='0'/>" ;

: HULL-EDGES ( -- n n n n )
   H-SEEN @ 0= if E-GLYPH throw then
   H-MINX @ IEEE754:BITS>F64 EDGE-LOW
   H-MINY @ IEEE754:BITS>F64 EDGE-LOW
   H-MAXX @ IEEE754:BITS>F64 EDGE-HIGH
   H-MAXY @ IEEE754:BITS>F64 EDGE-HIGH ;
: EXACT-EDGE? ( n -- bool ) {: edge:n :}
   edge MAX-EXACT-F32 negate >= edge MAX-EXACT-F32 <= and ;
: CHECK-BOX ( n n n n -- n n n n )
   {: left:n top:n right:n bottom:n :}
   left EXACT-EDGE? top EXACT-EDGE? and
   right EXACT-EDGE? and bottom EXACT-EDGE? and
   right left - 0 > and bottom top - 0 > and
   right left - MAX-EXACT-F32 <= and
   bottom top - MAX-EXACT-F32 <= and 0= if E-RENDER throw then
   left top right bottom ;
: FINAL-BOX? ( ptr u8 n n n n -- bool )
   {: draw:ptr x0:n y0:n x1:n y1:n :}
   EXTENTS-BYTES MEM-ALLOC-BYTES {: box:ptr cap:n :}
   draw box DRAW-EXTENTS 0= if
      box cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
      false exit
   then
   box F32@ x0 s>f f=
   box 4 + F32@ y0 s>f f= and
   box 8 + F32@ x1 x0 - s>f f= and
   box 12 + F32@ y1 y0 - s>f f= and {: exact:bool :}
   box cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   exact ;

: BOX-RENDER ( ptr u8 ptr u8 ptr u8 n -- ptr u8 r r r r )
   {: font:ptr buf:ptr draw:ptr ink:n :}
   EXTENTS-BYTES MEM-ALLOC-BYTES {: box:ptr cap:n :}
   draw box DRAW-EXTENTS 0= if
      box cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
      ink 0<> if E-GLYPH throw then
      EMPTY-SVG$ 0 NULL-PTR NULL-PTR BLOB-CREATE
      0.0 0.0 0.0 0.0 exit
   then
   box F32@ EDGE-LOW {: left:n :}
   box 4 + F32@ EDGE-LOW {: top:n :}
   box F32@ box 8 + F32@ f+ EDGE-HIGH {: right:n :}
   box 4 + F32@ box 12 + F32@ f+ EDGE-HIGH {: bottom:n :}
   box cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES

   \ The first render contains the exact decimal path coordinates. Rendering
   \ clears the draw context, so the shaped run is drawn once more below.
   draw DRAW-RENDER dup NULL? if E-RENDER throw then BLOB-CELL !
   BLOB-CELL @ NULL-PTR BLOB-DATA BLOB-CELL @ BLOB-LEN SCAN-SVG
   BLOB-CELL @ BLOB-DESTROY NULL-PTR BLOB-CELL !
   HULL-EDGES {: hx:n hy:n hr:n hb:n :}

   font buf draw DRAW-ALL
   left hx min top hy min right hr max bottom hb max CHECK-BOX
      {: x0:n y0:n x1:n y1:n :}
   EXTENTS-BYTES MEM-ALLOC-BYTES {: final-box:ptr final-cap:n :}
   x0 s>f final-box F32!
   y0 s>f final-box 4 + F32!
   x1 x0 - s>f final-box 8 + F32!
   y1 y0 - s>f final-box 12 + F32!
   draw final-box DRAW-SET-EXTENTS
   final-box final-cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   draw x0 y0 x1 y1 FINAL-BOX? 0= if E-RENDER throw then
   draw DRAW-RENDER
   x0 s>f y1 negate s>f x1 x0 - s>f y1 y0 - s>f ;

: DRAW-RUN ( ptr u8 ptr u8 ptr u8 ptr u8 n n n -- ptr u8 r r r r )
   {: font:ptr buf:ptr draw:ptr data:ptr size:n em:n ink:n :}
   font em 64 * dup FONT-SCALE
   buf data size 0 size BUF-ADD
   buf BUF-GUESS
   buf BUF-DIRECTION 4 <> if E-TEXT throw then
   font buf NULL-PTR 0 SHAPE
   draw 4 DRAW-PRECISION
   font buf draw DRAW-ALL
   font buf draw ink BOX-RENDER ;

: CLEAR-STATE ( -- )
   NULL-PTR REF-CELL ! NULL-PTR FONT-CELL ! NULL-PTR BUF-CELL !
   NULL-PTR DRAW-CELL ! NULL-PTR BLOB-CELL ! ;

: RELEASE-STATE ( -- )
   BLOB-CELL @ dup NULL? 0= if BLOB-DESTROY else drop then
   DRAW-CELL @ dup NULL? 0= if DRAW-DESTROY else drop then
   BUF-CELL @ dup NULL? 0= if BUF-DESTROY else drop then
   FONT-CELL @ dup NULL? 0= if FONT-DESTROY else drop then
   REF-CELL @ dup NULL? 0= if FACE-DESTROY else drop then
   CLEAR-STATE ;

: RENDER-BODY ( ptr u8 n n n -- image )
   {: data:ptr size:n em:n ink:n :}
   REF-CELL @ FONT-CREATE dup NULL? if E-RENDER throw then FONT-CELL !
   BUF-CREATE dup NULL? if E-RENDER throw then BUF-CELL !
   SVG-FORMAT DRAW-CREATE dup NULL? if E-RENDER throw then DRAW-CELL !
   FONT-CELL @ BUF-CELL @ DRAW-CELL @ data size em ink DRAW-RUN
   {: blob:ptr x:r y:r w:r h:r :}
   blob NULL? if E-RENDER throw then
   blob BLOB-CELL !
   blob x y w h KEEP-IMAGE
   NULL-PTR BLOB-CELL ! ;

: RENDER-REF ( ptr u8 ptr u8 n n n -- image )
   {: face:ptr data:ptr size:n em:n ink:n :}
   CLEAR-STATE
   face REF-CELL !
   data size em ink [: RENDER-BODY ;] [: RELEASE-STATE ;] finally ;

: NODE-BOUNDS ( ptr n -- r r r r ) {: node:ptr :}
   node BX @ IEEE754:BITS>F64
   node BY @ IEEE754:BITS>F64
   node BW @ IEEE754:BITS>F64
   node BH @ IEEE754:BITS>F64 ;

public

: OPEN ( ptr u8 n n -- font )
   {: data:ptr size:n index:n :}
   size 0 <= size MAX-FONT > or if E-OPEN throw then
   index 0 < index MAX-FONT > or if E-OPEN throw then
   data size index OPEN-BLOB OPEN-CELL !
   [: KEEP-OPEN ;] [: RELEASE-OPEN ;] finally ;
: CLOSE ( font -- )
   LOCK [: FONT-NODE dup FONTS REMOVE ;] [: UNLOCK ;] finally
      {: node:ptr :}
   node OBJECT @ FACE-DESTROY node DROP-NODE ;
: RENDER ( font ptr u8 n n -- image )
   {: handle:font data:ptr size:n em:n :}
   data size TEXT-CHECK {: ink:n :}
   em EM-CHECK drop
   handle FONT-REF data size em ink RENDER-REF ;
: SVG$ ( image -- ptr u8 n )
   LOCK
   [: IMAGE-NODE OBJECT @
      dup NULL-PTR BLOB-DATA swap BLOB-LEN
   ;] [: UNLOCK ;] finally ;
: BOUNDS ( image -- r r r r )
   LOCK
   [: IMAGE-NODE NODE-BOUNDS ;] [: UNLOCK ;] finally ;
: FREE ( image -- )
   LOCK [: IMAGE-NODE dup IMAGES REMOVE ;] [: UNLOCK ;] finally
      {: node:ptr :}
   node OBJECT @ BLOB-DESTROY node DROP-NODE ;

;package
