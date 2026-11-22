split-str
\ studio.f - STUDIO: the key light's volume, the bodies a module sends so the
\ host's ground shadow is the whole model's. KEY-VOLUME computes, in binary64:
\ - the view's axes as viewport.js's viewer gives them, then studio's (up the
\   plane's unit normal n), then keyAxes' key x, y and z;
\ - V, the plane's points at clip x, y = +-1 met along the view's back, and
\   R, their rectangle in key x, y; T, the highest rise n.p + d of the box's
\   corners; M, R grown in each key axis by the shadow's read reach, the
\   softbox's half extent there (KEY-BOX-X, KEY-BOX-Y) times D = T / nz, nz
\   n's key z, and cut to the rectangle of the box's corners in key x, y;
\ - the volume, every point on or above the plane whose key x, y lie in M, and
\   the key's pixel, M's larger side over the shadow map's side.
\ src/lib/browser/viewport.js states KEY, KEY_BOX, SHADOW_SIDE and studio's
\ turntable rule a second time: test/browser/display-peer.mjs compares this
\ volume's planes and pixel, as numbers, with the ground, M and pixel of the
\ shadow map the host fits, for two views; studio-test.f's expected numbers
\ come from viewport.js's own functions and constants, SHADOW_SIDE among them.
\ STORAGE CLASS. CALLER-OWNED: the planes go to the caller's room; no state.

require src/lib/error.f
require src/lib/fmath.f
require src/lib/float32.f
require src/lib/float32-buffer.f

package STUDIO
public

-9920 constant E-FIRST
-9929 constant E-LAST
-9920 s" a matrix that is not an invertible affine map" ERROR:CODE E-MATRIX

private

STRUCTURE vec FIELD x r FIELD y r FIELD z r ;STRUCTURE

\ The key light's direction and its softbox's half extents per unit along it,
\ in studio coordinates; the shadow map's largest side, the most at which
\ 255 side^2 still fits a u32 table's sums.
: KEY ( -- vec ) -0.4 0.6 0.7 VEC-MAKE ;
: KEY-BOX-X ( -- r ) 0.45 ;
: KEY-BOX-Y ( -- r ) 0.27 ;
4294967296 255 / FMATH:ISQRT-FLOOR constant SHADOW-SIDE

\ studio's least length of the view's right laid on the plane.
: LAID-LEAST ( -- r ) 0.000001 ;

: DOT ( vec vec -- r )
   {: a:vec b:vec :}
   a VEC-UNMAKE {: ax:r ay:r az:r :}
   b VEC-UNMAKE {: bx:r by:r bz:r :}
   ax bx f*  ay by f* f+  az bz f* f+ ;

: CROSS ( vec vec -- vec )
   {: a:vec b:vec :}
   a VEC-UNMAKE {: ax:r ay:r az:r :}
   b VEC-UNMAKE {: bx:r by:r bz:r :}
   ay bz f* az by f* f-
   az bx f* ax bz f* f-
   ax by f* ay bx f* f- VEC-MAKE ;

: SCALED ( vec r -- vec )
   {: a:vec k:r :}
   a VEC-UNMAKE {: ax:r ay:r az:r :}
   ax k f*  ay k f*  az k f* VEC-MAKE ;

: PLUS ( vec vec -- vec )
   {: a:vec b:vec :}
   a VEC-UNMAKE {: ax:r ay:r az:r :}
   b VEC-UNMAKE {: bx:r by:r bz:r :}
   ax bx f+  ay by f+  az bz f+ VEC-MAKE ;

: MINUS ( vec vec -- vec ) -1.0 SCALED PLUS ;

: LENGTH ( vec -- r ) dup DOT fsqrt ;

: UNIT ( vec -- vec ) dup LENGTH 1.0 swap f/ SCALED ;

\ a less its part along the unit b.
: REJECT ( vec vec -- vec )
   {: a:vec b:vec :}
   a  b a b DOT SCALED  MINUS ;

\ -1.0 for a negative r, else 1.0.
: SIGN ( r -- r ) f0< if -1.0 else 1.0 then ;

\ The k-th f32 of the span, widened.
: F32@ ( ^u8 n -- r ) 4 * + F32-BUF:LOAD F32:WIDEN ;

\ Row r of the column-major matrix m's 3x3 part, and its translation.
: ROW ( ^u8 n -- vec )
   {: m r:n :}
   m r F32@  m r 4 + F32@  m r 8 + F32@ VEC-MAKE ;
: SHIFT ( ^u8 n -- r ) 12 + F32@ ;

\ Whether m's fourth row is 0 0 0 1.
: AFFINE? ( ^u8 -- bool )
   {: m :}
   m 3 F32@ f0=  m 7 F32@ f0= and
   m 11 F32@ f0= and  m 15 F32@ 1.0 f= and ;

: INVERTIBLE? ( r -- bool )
   {: det:r :}
   det f0= 0=  det FMATH:FINITE? and ;

\ The columns of the inverse of m's 3x3 part, r1 x r2, r2 x r0 and r0 x r1
\ over the determinant, which an affine m's inverse holds; E-MATRIX for an m
\ that is not affine or not invertible.
: COLUMNS ( ^u8 -- vec vec vec )
   {: m :}
   m AFFINE? 0= if E-MATRIX throw then
   m 0 ROW {: r0:vec :}  m 1 ROW {: r1:vec :}  m 2 ROW {: r2:vec :}
   r1 r2 CROSS {: c0:vec :}
   r0 c0 DOT {: det:r :}
   det INVERTIBLE? 0= if E-MATRIX throw then
   1.0 det f/ {: k:r :}
   c0 k SCALED  r2 r0 CROSS k SCALED  r0 r1 CROSS k SCALED ;

\ viewer's back from the inverse's third column: that column negated, toward
\ lower depth, so only the depth row's sign reaches it.
: BACK ( vec -- vec ) UNIT -1.0 SCALED ;

\ viewer's right, up and back from the inverse's columns: right and up the
\ first two less their parts along back, and up along right.
: VIEWER ( vec vec vec -- vec vec vec )
   {: across:vec above:vec deep:vec :}
   deep BACK {: back:vec :}
   across back REJECT UNIT {: right:vec :}
   above back REJECT  right above right DOT SCALED  MINUS UNIT
   right swap back ;

\ studio's right, the view's right laid on the plane of unit normal n, or,
\ where that lies along n, s times n x the view's back laid on it.
: STUDIO-RIGHT ( vec vec vec r -- vec )
   {: right:vec back:vec n:vec s:r :}
   right n REJECT {: along:vec :}
   along LENGTH LAID-LEAST f< if
      n  back n REJECT  CROSS UNIT s SCALED exit
   then
   along UNIT ;

\ studio's right, up and back for the view's right, up and back and the
\ plane's unit normal n: up is n, back s right x n, s the sign of the view's
\ right . (up x back), so the studio keeps the view's handedness.
: STUDIO-AXES ( vec vec vec vec -- vec vec vec )
   {: right:vec up:vec back:vec n:vec :}
   right  up back CROSS  DOT SIGN {: s:r :}
   right back n s STUDIO-RIGHT {: r:vec :}
   r  n  r n CROSS s SCALED ;

\ The studio direction a in world, by the studio's axes.
: WORLD ( vec vec vec vec -- vec )
   {: a:vec right:vec up:vec back:vec :}
   a VEC-UNMAKE {: ax:r ay:r az:r :}
   right ax SCALED  up ay SCALED PLUS  back az SCALED PLUS ;

\ keyAxes' x, y and z in world: the softbox's tangents and the way toward
\ the key, for the studio's right, up and back.
: KEY-AXES ( vec vec vec -- vec vec vec )
   {: right:vec up:vec back:vec :}
   KEY UNIT {: c:vec :}
   0.0 1.0 0.0 VEC-MAKE c CROSS UNIT {: across:vec :}
   across right up back WORLD
   c across CROSS right up back WORLD
   c right up back WORLD ;

\ The plane's normal; its unit normal, and d over the normal's length.
: PLANE-NORMAL ( ^u8 -- vec )
   {: g :}
   g 0 F32@  g 1 F32@  g 2 F32@ VEC-MAKE ;
: NORMAL ( ^u8 -- vec ) PLANE-NORMAL UNIT ;
: OFFSET ( ^u8 -- r )
   {: g :}
   g 3 F32@  g PLANE-NORMAL LENGTH f/ ;

\ The point q, or with d 0.0 the direction q, moved along back onto the
\ plane n.p + d = 0.
: ONTO ( vec vec vec r -- vec )
   {: q:vec back:vec n:vec d:r :}
   q  back  n q DOT d f+  n back DOT f/  SCALED  MINUS ;

\ Whether m's eye, the way of viewer's back, is above g's plane.
: ABOVE? ( ^u8 ^u8 -- bool )
   {: m g :}
   m COLUMNS {: cx:vec cy:vec cz:vec :}
   g NORMAL  cz BACK  DOT 0.0 f> ;

\ The key's x, y and z for m's view of g's plane.
: KEY-FRAME ( ^u8 ^u8 -- vec vec vec )
   {: m g :}
   m COLUMNS VIEWER  g NORMAL  STUDIO-AXES KEY-AXES ;

\ V as p0 + x ex + y ey for clip x, y = +-1: p0 is the plane's point at
\ clip x, y = 0, ex and ey its steps per unit of clip x and y.
: GROUND ( ^u8 ^u8 -- vec vec vec )
   {: m g :}
   m COLUMNS {: cx:vec cy:vec cz:vec :}
   cz BACK {: back:vec :}
   g NORMAL {: n:vec :}
   cx m 0 SHIFT fnegate SCALED  cy m 1 SHIFT fnegate SCALED  PLUS
   back n g OFFSET ONTO
   cx back n 0.0 ONTO  cy back n 0.0 ONTO ;

\ R in key axis k: V's corners p0 +- ex +- ey span k.p0 +- (|k.ex| + |k.ey|).
: VIEW-SPAN ( vec vec vec vec -- r r )
   {: k:vec p0:vec ex:vec ey:vec :}
   k p0 DOT {: mid:r :}
   k ex DOT fabs  k ey DOT fabs f+ {: half:r :}
   mid half f-  mid half f+ ;

\ Coordinate j of the box's corner i, bit j of i picking the highest.
: COORD ( ^u8 n n -- r )
   {: g i:n j:n :}
   g  i j rshift 1 and 3 *  j + 4 +  F32@ ;

: CORNER ( ^u8 n -- vec )
   {: g i:n :}
   g i 0 COORD  g i 1 COORD  g i 2 COORD VEC-MAKE ;

\ T, the highest rise n.p + d of the box's corners.
: RISE ( ^u8 n -- r )
   {: g i:n :}
   g NORMAL  g i CORNER  DOT  g OFFSET f+ ;

: TALLEST ( ^u8 -- r )
   {: g :}
   g 0 RISE
   8 1 ?do g i RISE FMATH:FMAX loop ;

\ The lowest and highest of the box's corners in key axis k.
: BOX-SPAN ( ^u8 vec -- r r )
   {: g k:vec :}
   g 0 CORNER k DOT {: first:r :}
   first first
   8 1 ?do
      {: lo:r hi:r :}
      g i CORNER k DOT {: v:r :}
      lo v FMATH:FMIN  hi v FMATH:FMAX
   loop ;

\ The shadow's read reach in a key axis for the softbox's half extent h
\ there: h D, D = T / nz and nz = n.kz, how far from a ground point the
\ shadow's summed-table reads reach in that axis.
: GROWTH ( ^u8 vec r r -- r )
   {: g kz:vec t:r h:r :}
   g NORMAL kz DOT {: nz:r :}
   h t nz f/ f* ;

\ M in key axis k: R grown by grow, cut to the box's span.
: M-SPAN ( ^u8 ^u8 vec r -- r r )
   {: m g k:vec grow:r :}
   m g GROUND {: p0:vec ex:vec ey:vec :}
   k p0 ex ey VIEW-SPAN {: lo:r hi:r :}
   g k BOX-SPAN {: blo:r bhi:r :}
   lo grow f- blo FMATH:FMAX  hi grow f+ bhi FMATH:FMIN ;

\ M's lowest and highest key x, then key y, for m's view of g and T t.
: M ( ^u8 ^u8 r -- r r r r )
   {: m g t:r :}
   m g KEY-FRAME {: kx:vec ky:vec kz:vec :}
   m g kx  g kz t KEY-BOX-X GROWTH  M-SPAN
   m g ky  g kz t KEY-BOX-Y GROWTH  M-SPAN ;

\ Whether M holds no point; a NaN side holds none.
: EMPTY? ( r r r r -- bool )
   {: xlo:r xhi:r ylo:r yhi:r :}
   xlo xhi f<=  ylo yhi f<=  and 0= ;

\ Plane i of out, a b c d.
: PLANE! ( vec r ^r n -- )
   {: a:vec d:r out:^ i:n :}
   out i 4 * cells + {: p:^ :}
   a VEC-UNMAKE {: x:r y:r z:r :}
   x p !  y p cell+ !  z p 2 cells + !  d p 3 cells + ! ;

\ Planes i and i + 1 of out: key axis k at least lo, then at most hi.
: SIDES! ( vec r r ^r n -- )
   {: k:vec lo:r hi:r out:^ i:n :}
   k lo fnegate out i PLANE!
   k -1.0 SCALED hi out i 1+ PLANE! ;

\ The five planes of the volume into out.
: PLANES! ( ^u8 ^u8 r r r r ^r -- )
   {: m g xlo:r xhi:r ylo:r yhi:r out:^ :}
   m g KEY-FRAME {: kx:vec ky:vec kz:vec :}
   g NORMAL g OFFSET out 0 PLANE!
   kx xlo xhi out 1 SIDES!
   ky ylo yhi out 3 SIDES! ;

\ The key's pixel: M's larger side over the shadow map's side, the drawing
\ buffer's larger side w or h, at most SHADOW-SIDE.
: PIXEL ( r r r r n n -- r )
   {: xlo:r xhi:r ylo:r yhi:r w:n h:n :}
   xhi xlo f-  yhi ylo f-  FMATH:FMAX
   w h max SHADOW-SIDE min s>f f/ ;

public

\ The key light's volume for the draw's matrix m, 16 f32 column-major as
\ the draw record carries it, and the ground g, ten f32: the plane nx ny nz
\ d, then the box no caster leaves, lowest x y z then highest; for the
\ drawing buffer's width and height. Writes into out, room for 20 binary64,
\ five planes a b c d, a point inside when a x + b y + c z + d >= 0: the
\ ground, then key x at least M's lowest, at most its highest, then key y
\ the same; answers 5 and the key's pixel in world units. Answers 0 and 0.0,
\ out's contents no planes, when the eye is not above the plane, T is not
\ above 0 or M is empty. A casters' box that is a point answers 5 and 0.0,
\ where viewport.js draws no shadow; no model gives one. Reads only the
\ direction of m's depth row. E-MATRIX for an m whose fourth row is not
\ 0 0 0 1 or that is not invertible.
: KEY-VOLUME ( ^u8 ^u8 n n ^r -- n r )
   {: m g w:n h:n out:^ :}
   m g ABOVE? 0= if 0 0.0 exit then
   g TALLEST {: t:r :}
   t 0.0 f> 0= if 0 0.0 exit then
   m g t M {: xlo:r xhi:r ylo:r yhi:r :}
   xlo xhi ylo yhi EMPTY? if 0 0.0 exit then
   m g xlo xhi ylo yhi out PLANES!
   5  xlo xhi ylo yhi w h PIXEL ;

;package
