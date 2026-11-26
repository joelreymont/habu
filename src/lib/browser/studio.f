\ studio.f - STUDIO: the key light's fit, the ground record a module sends the
\ host and the volume it culls its casters to, so the host's ground shadow is
\ the whole model's. KEY-FIT computes, in binary64:
\ - the view's right, up and back, then the studio's (up the plane's unit
\   normal n), then the key's x, y and z, the light's tangents and direction
\   laid on the studio's axes;
\ - V, the plane's points at clip x, y = +-1 met along the view's back, and
\   R, their rectangle in key x, y; T, the highest rise n.p + d of the box's
\   corners; M, R grown by how far its rays to the softbox go in key x and y
\   before they rise T, and cut to the rectangle of the box's corners there;
\   C, M grown the same reach the other way: the ground that sees M on its
\   way to the softbox;
\ - the volume, every point on or above the plane whose key x, y lie in M, and
\   the key's pixel, M's larger side over the shadow map's side.
\ Each step reads the f32 numbers the steps before it wrote into the record,
\ so the volume culled is the volume the host casts from the record.
\ STORAGE CLASS. CALLER-OWNED: the record and the planes go to the caller's
\ room; no state.

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

\ The k-th f32 of the span, widened; r rounded into it.
: F32@ ( ^u8 n -- r ) 4 * + F32-BUF:LOAD F32:WIDEN ;
: F32! ( r ^u8 n -- )
   {: v:r p i:n :}
   v F32:NARROW  p i 4 * +  F32-BUF:STORE ;

\ The f32s k, k + 1 and k + 2 of the span.
: VEC@ ( ^u8 n -- vec )
   {: p k:n :}
   p k F32@  p k 1+ F32@  p k 2 + F32@ VEC-MAKE ;
: VEC! ( vec ^u8 n -- )
   {: a:vec p k:n :}
   a VEC-UNMAKE {: x:r y:r z:r :}
   x p k F32!  y p k 1+ F32!  z p k 2 + F32! ;

\ The light's softbox half extents in key x and y, per unit along the key,
\ and the shadow map's side.
: BOX-X ( ^u8 -- r ) 9 F32@ ;
: BOX-Y ( ^u8 -- r ) 10 F32@ ;
: SIDE ( ^u8 -- r ) 11 F32@ ;

\ The record's plane, unit normal and d, and the studio's right and back.
: NORMAL@ ( ^u8 -- vec ) 0 VEC@ ;
: OFFSET@ ( ^u8 -- r ) 3 F32@ ;
: RIGHT@ ( ^u8 -- vec ) 4 VEC@ ;
: BACK@ ( ^u8 -- vec ) 7 VEC@ ;

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

\ The key's x, y and z in world: the light's softbox tangents and way toward
\ the key, for the studio's right, up and back.
: KEY-AXES ( ^u8 vec vec vec -- vec vec vec )
   {: light right:vec up:vec back:vec :}
   light 0 VEC@ right up back WORLD
   light 3 VEC@ right up back WORLD
   light 6 VEC@ right up back WORLD ;

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

\ The record's plane and studio axes for m's view of g's plane.
: AXES! ( ^u8 ^u8 ^u8 -- )
   {: m g rec :}
   g NORMAL {: n:vec :}
   m COLUMNS VIEWER n STUDIO-AXES {: r:vec u:vec b:vec :}
   n rec 0 VEC!  g OFFSET rec 3 F32!
   r rec 4 VEC!  b rec 7 VEC! ;

\ Whether m's eye, the way of viewer's back, is above the record's plane.
: ABOVE? ( ^u8 ^u8 -- bool )
   {: m rec :}
   m COLUMNS {: cx:vec cy:vec cz:vec :}
   rec NORMAL@  cz BACK  DOT 0.0 f> ;

\ The key's x, y and z by the record's studio axes.
: KEY-FRAME ( ^u8 ^u8 -- vec vec vec )
   {: light rec :}
   light  rec RIGHT@  rec NORMAL@  rec BACK@  KEY-AXES ;

\ V as p0 + x ex + y ey for clip x, y = +-1: p0 is the plane's point at
\ clip x, y = 0, ex and ey its steps per unit of clip x and y.
: GROUND ( ^u8 ^u8 -- vec vec vec )
   {: m rec :}
   m COLUMNS {: cx:vec cy:vec cz:vec :}
   cz BACK {: back:vec :}
   rec NORMAL@ {: n:vec :}
   cx m 0 SHIFT fnegate SCALED  cy m 1 SHIFT fnegate SCALED  PLUS
   back n rec OFFSET@ ONTO
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

\ T, the highest rise n.p + d of g's box's corners over the record's plane.
: RISE ( ^u8 ^u8 n -- r )
   {: rec g i:n :}
   rec NORMAL@  g i CORNER  DOT  rec OFFSET@ f+ ;

: TALLEST ( ^u8 ^u8 -- r )
   {: rec g :}
   rec g 0 RISE
   8 1 ?do rec g i RISE FMATH:FMAX loop ;

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

\ How far along the key the rays at the softbox's height v rise t above
\ the plane: they rise nz + v ny a unit, ny and nz n.ky and n.kz, n having
\ no part along key x.
: RISEN ( ^u8 vec vec r r -- r )
   {: rec ky:vec kz:vec t:r v:r :}
   rec NORMAL@ {: n:vec :}
   t  n kz DOT  n ky DOT v f*  f+ f/ ;

\ How far a ground point's rays to the softbox go before they rise t, in key
\ x either way, in key y down and up: those at its low edge go furthest.
: GROWTH ( ^u8 ^u8 vec vec r -- r r r )
   {: light rec ky:vec kz:vec t:r :}
   light BOX-Y {: by:r :}
   rec ky kz t by fnegate RISEN {: far:r :}
   light BOX-X far f*  by far f*
   by  rec ky kz t by RISEN f* ;

\ M in key axis k: R grown by down and up, cut to the box's span.
: M-SPAN ( ^u8 ^u8 ^u8 vec r r -- r r )
   {: m rec g k:vec down:r up:r :}
   m rec GROUND {: p0:vec ex:vec ey:vec :}
   k p0 ex ey VIEW-SPAN {: lo:r hi:r :}
   g k BOX-SPAN {: blo:r bhi:r :}
   lo down f- blo FMATH:FMAX  hi up f+ bhi FMATH:FMIN ;

\ Four f32 of the record from slot k: lowest and highest key x, then key y.
: RECT@ ( ^u8 n -- r r r r )
   {: rec k:n :}
   rec k F32@  rec k 1+ F32@  rec k 2 + F32@  rec k 3 + F32@ ;
: RECT! ( r r r r ^u8 n -- )
   {: xlo:r xhi:r ylo:r yhi:r rec k:n :}
   xlo rec k F32!  xhi rec k 1+ F32!
   ylo rec k 2 + F32!  yhi rec k 3 + F32! ;

: M@ ( ^u8 -- r r r r ) 10 RECT@ ;
: T@ ( ^u8 -- r ) 18 F32@ ;

\ M into the record, its key x grown by across, its key y by down and up.
: M! ( ^u8 ^u8 ^u8 vec vec r r r -- )
   {: m g rec kx:vec ky:vec across:r down:r up:r :}
   m rec g kx across across M-SPAN
   m rec g ky down up M-SPAN  rec 10 RECT! ;

\ C into the record: the record's M grown by across, and by up below and
\ down above.
: CATCHER! ( ^u8 r r r -- )
   {: rec across:r down:r up:r :}
   rec M@ {: xlo:r xhi:r ylo:r yhi:r :}
   xlo across f-  xhi across f+
   ylo up f-  yhi down f+  rec 14 RECT! ;

\ The casters' nearest and farthest key z into the record.
: DEPTHS! ( ^u8 ^u8 vec -- )
   {: g rec kz:vec :}
   g kz BOX-SPAN {: far:r near:r :}
   near rec 19 F32!  far rec 20 F32! ;

\ M, C, near and far into the record, for the record's T.
: SPANS! ( ^u8 ^u8 ^u8 ^u8 -- )
   {: light m g rec :}
   light rec KEY-FRAME {: kx:vec ky:vec kz:vec :}
   light rec ky kz rec T@ GROWTH {: across:r down:r up:r :}
   m g rec kx ky across down up M!
   rec across down up CATCHER!
   g rec kz DEPTHS! ;

\ Whether M holds no point; a NaN side holds none.
: EMPTY? ( r r r r -- bool )
   {: xlo:r xhi:r ylo:r yhi:r :}
   xlo xhi f<=  ylo yhi f<=  and 0= ;

\ The record's M's larger side.
: LARGER ( ^u8 -- r )
   {: rec :}
   rec M@ {: xlo:r xhi:r ylo:r yhi:r :}
   xhi xlo f-  yhi ylo f-  FMATH:FMAX ;

\ Whether the record's M holds a point and M or the casters' depth along
\ the key has extent: a casters' box that is a point casts nothing.
: CAST? ( ^u8 -- bool )
   {: rec :}
   rec M@ EMPTY? if false exit then
   rec LARGER  rec 19 F32@ rec 20 F32@ f-  FMATH:FMAX  0.0 f> ;

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

\ The five planes of the record's volume into out.
: PLANES! ( ^u8 ^u8 ^r -- )
   {: light rec out:^ :}
   light rec KEY-FRAME {: kx:vec ky:vec kz:vec :}
   rec M@ {: xlo:r xhi:r ylo:r yhi:r :}
   rec NORMAL@ rec OFFSET@ out 0 PLANE!
   kx xlo xhi out 1 SIDES!
   ky ylo yhi out 3 SIDES! ;

public

\ The key light's fit for the light, 48 bytes, twelve f32 in studio
\ coordinates (right, up, back): the softbox's x tangent, its y tangent, the
\ unit direction toward the key, the softbox's half extents in x and y per
\ unit along it, the shadow map's side in texels, its x tangent square to
\ the studio's up and every ray to its softbox rising; the draw's matrix m,
\ 16 f32 column-major as the draw record carries it; and the ground g, ten
\ f32, each finite, else the fit is undefined: the plane nx ny nz d, then
\ the box no caster leaves, lowest x y z then highest. Writes into rec, room
\ for 84 bytes, the ground record: ten f32, the plane's unit normal and d,
\ then the studio's right and back in world; and, with a volume, M (key x
\ lowest, highest, key y lowest, highest), C in the same order, T, and the
\ casters' nearest and farthest key z. Writes into out, room for 20
\ binary64, five planes a b c d, a point inside when a x + b y + c z + d >=
\ 0: the ground, then key x at least M's lowest, at most its highest, then
\ key y the same. Answers the record's length, 84, 5 planes and the key's
\ pixel in world units; or 40, 0 and 0.0 when the eye is not above the
\ plane, T is not above 0, M is empty, or neither M nor the casters' depth
\ along the key has extent, as for a casters' box that is a point. Reads
\ only the direction of m's depth row. E-MATRIX for an m whose fourth row
\ is not 0 0 0 1 or that is not invertible.
: KEY-FIT ( ^u8 ^u8 ^u8 ^u8 ^r -- n n r )
   {: light m g rec out:^ :}
   m g rec AXES!
   m rec ABOVE? 0= if 40 0 0.0 exit then
   rec g TALLEST rec 18 F32!
   rec T@ 0.0 f> 0= if 40 0 0.0 exit then
   light m g rec SPANS!
   rec CAST? 0= if 40 0 0.0 exit then
   light rec out PLANES!
   84 5  rec LARGER light SIDE f/ ;

;package
