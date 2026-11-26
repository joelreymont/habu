\ studio-test.f - STUDIO:KEY-FIT's record, planes and pixel for scenes stated
\ here, each number within 1e-9 of the larger of its own size and 1, or
\ 1e-6 in SLOPED, whose light's 0.8 and 0.6 an f32 rounds. Every view but
\ BELOW's looks down from +z with its right along +x. Every light but
\ SLOPED's stands straight overhead, a square softbox of half extent 0.5
\ and a map of side 1024, so key x, y and z are world x, y and z: M is R
\ grown by T / 2 each way and cut to the box's rectangle, C is M grown by
\ T / 2 again, and the pixel is M's larger side over 1024. The eye below the plane, a box under it, a
\ ground whose M is empty and a box that is a point answer the 40-byte
\ record alone. A matrix whose fourth row is not 0 0 0 1, and a singular
\ one, are E-MATRIX.
\ Run: bin/hb --load src/lib/browser/studio-test.f

require src/lib/test/assert.f
require src/lib/fmath.f
require src/lib/float32.f
require src/lib/float32-buffer.f
require src/lib/browser/studio.f

package STUDIO-TEST
private

4 constant F32-BYTES
12 F32-BYTES * BUFFER: LIGHT
16 F32-BYTES * BUFFER: MATRIX
10 F32-BYTES * BUFFER: GROUND
21 F32-BYTES * BUFFER: RECORD
20 TYPED-BUFFER PLANES r
TYPED-VARIABLE TOLERANCE r

: F32! ( r ^u8 n -- )
   {: v:r p i:n :}
   v F32:NARROW  p i F32-BYTES * +  F32-BUF:STORE ;

: F32@ ( ^u8 n -- r )
   F32-BYTES * + F32-BUF:LOAD F32:WIDEN ;

: TRIPLE! ( r r r ^u8 n -- )
   {: x:r y:r z:r p i:n :}
   x p i F32!  y p i 1+ F32!  z p i 2 + F32! ;

\ Key x the studio's right, key y its front, -back, and the key up; half
\ extents 0.5 by 0.5; side 1024.
: LIGHT! ( -- )
   1.0 0.0 0.0 LIGHT 0 TRIPLE!
   0.0 0.0 -1.0 LIGHT 3 TRIPLE!
   0.0 1.0 0.0 LIGHT 6 TRIPLE!
   0.5 0.5 1024.0 LIGHT 9 TRIPLE! ;

\ Key x the studio's right, key y up 0.6 and back -0.8, the key up 0.8 and
\ back 0.6; half extents 0.5 by 1/3; side 1024.
: SLOPED-LIGHT! ( -- )
   1.0 0.0 0.0 LIGHT 0 TRIPLE!
   0.0 0.6 -0.8 LIGHT 3 TRIPLE!
   0.0 0.8 0.6 LIGHT 6 TRIPLE!
   0.5 1.0 3.0 f/ 1024.0 LIGHT 9 TRIPLE! ;

: EXACT ( -- ) 0.000000001 TOLERANCE ! ;
: ROUNDED ( -- ) 0.000001 TOLERANCE ! ;

\ Row i of the world-to-clip map, a x + b y + c z + t, column-major.
: ROW! ( r r r r n -- )
   {: a:r b:r c:r t:r i:n :}
   a MATRIX i F32!  b MATRIX i 4 + F32!
   c MATRIX i 8 + F32!  t MATRIX i 12 + F32! ;

: AFFINE! ( -- ) 0.0 0.0 0.0 1.0 3 ROW! ;

\ Clip x and y s x + t and s y + t, depth k z + 0.5.
: VIEW! ( r r r -- )
   {: s:r t:r k:r :}
   s 0.0 0.0 t 0 ROW!  0.0 s 0.0 t 1 ROW!
   0.0 0.0 k 0.5 2 ROW!  AFFINE! ;

: PLANE! ( r r r r -- )
   {: a:r b:r c:r d:r :}
   a GROUND 0 F32!  b GROUND 1 F32!  c GROUND 2 F32!  d GROUND 3 F32! ;

\ The box no caster leaves, lowest x y z then highest.
: BOX! ( r r r r r r -- )
   {: x0:r y0:r z0:r x1:r y1:r z1:r :}
   x0 y0 z0 GROUND 4 TRIPLE!  x1 y1 z1 GROUND 7 TRIPLE! ;

: NEAR ( r r -- )
   {: got:r want:r :}
   got want f- fabs
   want fabs 1.0 FMATH:FMAX TOLERANCE @ f*  f<= TTRUE ;

: PLANE= ( r r r r n -- )
   {: a:r b:r c:r d:r i:n :}
   i 4 * {: k:n :}
   k PLANES @ a NEAR  k 1+ PLANES @ b NEAR
   k 2 + PLANES @ c NEAR  k 3 + PLANES @ d NEAR ;

\ The record's f32s from slot k.
: AT= ( r n -- ) RECORD swap F32@ swap NEAR ;
: TRIPLE= ( r r r n -- )
   {: x:r y:r z:r k:n :}
   x k AT=  y k 1+ AT=  z k 2 + AT= ;
: RECT= ( r r r r n -- )
   {: a:r b:r c:r d:r k:n :}
   a k AT=  b k 1+ AT=  c k 2 + AT=  d k 3 + AT= ;

\ The plane z = 0, the studio's right +x and back -y.
: FLOOR= ( -- )
   0.0 0.0 1.0 0 TRIPLE=  0.0 3 AT=
   1.0 0.0 0.0 4 TRIPLE=  0.0 -1.0 0.0 7 TRIPLE= ;

: FIT ( -- n n r )
   LIGHT MATRIX GROUND RECORD 0 PLANES STUDIO:KEY-FIT ;

\ The 40-byte record alone.
: NONE= ( n n r -- )
   {: bytes:n count:n px:r :}
   bytes 40 T=  count 0 T=  px 0.0 NEAR ;

\ The box -3 -2 0 to 4 5 2 on the plane z = 0.
: SHELF! ( -- )
   0.0 0.0 1.0 0.0 PLANE!
   -3.0 -2.0 0.0 4.0 5.0 2.0 BOX! ;

\ The ground in view x, y in 0..4; T 2. M is x -1..5 cut to -3..4, y -1..5
\ within -2..5; C x -2..5, y -2..6; near 2, far 0; the pixel 6 / 1024.
: TOP ( -- )
   s" top" T-LABEL
   0.5 -1.0 -0.0625 VIEW!  SHELF!
   FIT {: bytes:n count:n px:r :}
   bytes 84 T=  count 5 T=  px 0.005859375 NEAR
   FLOOR=
   -1.0 4.0 -1.0 5.0 10 RECT=  -2.0 5.0 -2.0 6.0 14 RECT=
   2.0 18 AT=  2.0 19 AT=  0.0 20 AT=
   0.0 0.0 1.0 0.0 0 PLANE=
   1.0 0.0 0.0 1.0 1 PLANE=  -1.0 0.0 0.0 4.0 2 PLANE=
   0.0 1.0 0.0 1.0 3 PLANE=  0.0 -1.0 0.0 5.0 4 PLANE= ;

\ The plane 2 z - 2 = 0, z = 1, under the box -10 -0.5 1 to 10 0.5 3, the
\ ground in view x, y in -20..20; T 2. M is x -10..10, y -0.5..0.5, both cut
\ to the box; C x -11..11, y -1.5..1.5; near 3, far 1; the pixel 20 / 1024,
\ M's key x side.
: RAISED ( -- )
   s" raised" T-LABEL
   0.05 0.0 -0.0625 VIEW!
   0.0 0.0 2.0 -2.0 PLANE!
   -10.0 -0.5 1.0 10.0 0.5 3.0 BOX!
   FIT {: bytes:n count:n px:r :}
   bytes 84 T=  count 5 T=  px 0.01953125 NEAR
   0.0 0.0 1.0 0 TRIPLE=  -1.0 3 AT=
   -10.0 10.0 -0.5 0.5 10 RECT=  -11.0 11.0 -1.5 1.5 14 RECT=
   2.0 18 AT=  3.0 19 AT=  1.0 20 AT=
   0.0 0.0 1.0 -1.0 0 PLANE=
   1.0 0.0 0.0 10.0 1 PLANE=  -1.0 0.0 0.0 10.0 2 PLANE=
   0.0 1.0 0.0 0.5 3 PLANE=  0.0 -1.0 0.0 0.5 4 PLANE= ;

\ The eye on -z: the studio turns about its up to keep the view's
\ handedness, its right +x and back +y.
: BELOW ( -- )
   s" below" T-LABEL
   0.5 -1.0 0.0625 VIEW!  SHELF!
   FIT NONE=
   0.0 0.0 1.0 0 TRIPLE=  0.0 3 AT=
   1.0 0.0 0.0 4 TRIPLE=  0.0 1.0 0.0 7 TRIPLE= ;

\ The box -3 -2 -1 to 4 5 0, T 0.
: UNDER ( -- )
   s" under" T-LABEL
   0.5 -1.0 -0.0625 VIEW!
   0.0 0.0 1.0 0.0 PLANE!
   -3.0 -2.0 -1.0 4.0 5.0 0.0 BOX!
   FIT NONE=  FLOOR= ;

\ The ground in view x in 100..102: M's key x 99..103 cut to -3..4 is
\ empty.
: OUTSIDE ( -- )
   s" outside" T-LABEL
   1.0 0.0 0.0 -101.0 0 ROW!  0.0 1.0 0.0 -1.0 1 ROW!
   0.0 0.0 -0.0625 0.5 2 ROW!  AFFINE!  SHELF!
   FIT NONE=  FLOOR= ;

\ The box the point 1 1 1: M is that point and near equals far.
: POINT ( -- )
   s" point" T-LABEL
   0.5 -1.0 -0.0625 VIEW!
   0.0 0.0 1.0 0.0 PLANE!
   1.0 1.0 1.0 1.0 1.0 1.0 BOX!
   FIT NONE=  FLOOR= ;

\ TOP's view of the plane z = 0 under the box -3 -2 0 to 4 5 1.8, T 1.8,
\ the key rising toward the studio's back: key y 0.8 y + 0.6 z, key z
\ -0.6 y + 0.8 z. Rays to the softbox's low and high edges rise 0.8 - 0.6 /
\ 3 = 0.6 and 0.8 + 0.6 / 3 = 1.0 a unit along the key, so they rise T in 3
\ and 1.8: across 0.5 * 3 = 1.5, down 3 / 3 = 1, up 1.8 / 3 = 0.6. R is x
\ 0..4, key y 0..3.2; M x -1.5..5.5 cut to the box's -3..4, key y -1..3.8
\ within the box's -1.6..5.08; C x -3..5.5, key y -1.6..4.8; near 2.64 at
\ y -2, z 1.8, far -3 at y 5, z 0; the pixel 5.5 / 1024.
: SLOPED ( -- )
   s" sloped" T-LABEL
   SLOPED-LIGHT! ROUNDED
   0.5 -1.0 -0.0625 VIEW!
   0.0 0.0 1.0 0.0 PLANE!
   -3.0 -2.0 0.0 4.0 5.0 1.8 BOX!
   FIT {: bytes:n count:n px:r :}
   bytes 84 T=  count 5 T=  px 0.00537109375 NEAR
   FLOOR=
   -1.5 4.0 -1.0 3.8 10 RECT=  -3.0 5.5 -1.6 4.8 14 RECT=
   1.8 18 AT=  2.64 19 AT=  -3.0 20 AT=
   0.0 0.0 1.0 0.0 0 PLANE=
   1.0 0.0 0.0 1.5 1 PLANE=  -1.0 0.0 0.0 4.0 2 PLANE=
   0.0 0.8 0.6 1.0 3 PLANE=  0.0 -0.8 -0.6 3.8 4 PLANE=
   LIGHT! EXACT ;

: KEPT ( -- ) FIT {: bytes:n count:n px:r :} ;

\ TOP's matrix with a fourth row 0 0.25 0 1, then with rows 0 and 1
\ parallel.
: REJECTED ( -- )
   s" rejected" T-LABEL
   0.5 -1.0 -0.0625 VIEW!  0.0 0.25 0.0 1.0 3 ROW!  SHELF!
   [: KEPT ;] STUDIO:E-MATRIX TTHROWSQ
   1.0 0.0 0.0 -1.0 1 ROW!  AFFINE!
   [: KEPT ;] STUDIO:E-MATRIX TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   LIGHT!  EXACT
   TOP
   RAISED
   BELOW
   UNDER
   OUTSIDE
   POINT
   SLOPED
   REJECTED
   T-REPORT ;

;package

STUDIO-TEST:RUN
