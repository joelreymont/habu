\ studio-test.f - STUDIO:KEY-VOLUME for three scenes stated here, each number
\ within 1e-9 of the larger of its own size and 1; the expected numbers come
\ from viewport.js's own viewer, studio, keyAxes and keyVolume, run in
\ binary64 on the scene's f32 numbers. A matrix whose fourth row is not
\ 0 0 0 1, and a singular one, are E-MATRIX.
\ Run: bin/hb --load src/lib/browser/studio-test.f

require src/lib/test.f
require src/lib/fmath.f
require src/lib/float32.f
require src/lib/float32-buffer.f
require src/lib/browser/studio.f

package STUDIO-TEST
private

4 constant F32-BYTES
16 F32-BYTES * BUFFER: MATRIX
10 F32-BYTES * BUFFER: GROUND
20 TYPED-BUFFER PLANES r

: F32! ( r ^u8 n -- )
   {: v:r p i:n :}
   v F32:NARROW  p i F32-BYTES * +  F32-BUF:STORE ;

\ Row i of the world-to-clip map, a x + b y + c z + t, column-major.
: ROW! ( r r r r n -- )
   {: a:r b:r c:r t:r i:n :}
   a MATRIX i F32!  b MATRIX i 4 + F32!
   c MATRIX i 8 + F32!  t MATRIX i 12 + F32! ;

: AFFINE! ( -- ) 0.0 0.0 0.0 1.0 3 ROW! ;

: PLANE! ( r r r r -- )
   {: a:r b:r c:r d:r :}
   a GROUND 0 F32!  b GROUND 1 F32!  c GROUND 2 F32!  d GROUND 3 F32! ;

\ The box no caster leaves, lowest x y z then highest.
: BOX! ( r r r r r r -- )
   {: x0:r y0:r z0:r x1:r y1:r z1:r :}
   x0 GROUND 4 F32!  y0 GROUND 5 F32!  z0 GROUND 6 F32!
   x1 GROUND 7 F32!  y1 GROUND 8 F32!  z1 GROUND 9 F32! ;

: NEAR ( r r -- )
   {: got:r want:r :}
   got want f- fabs
   want fabs 1.0 FMATH:FMAX 0.000000001 f*  f<= TTRUE ;

: PLANE= ( r r r r n -- )
   {: a:r b:r c:r d:r i:n :}
   i 4 * {: k:n :}
   k PLANES @ a NEAR  k 1+ PLANES @ b NEAR
   k 2 + PLANES @ c NEAR  k 3 + PLANES @ d NEAR ;

: VOLUME ( n n -- n r )
   {: w:n h:n :}
   MATRIX GROUND w h 0 PLANES STUDIO:KEY-VOLUME ;

\ The box -3 -2 0 to 4 5 2 on the plane z = 0.
: SHELF! ( -- )
   0.0 0.0 1.0 0.0 PLANE!
   -3.0 -2.0 0.0 4.0 5.0 2.0 BOX! ;

\ Looking down on z = 0 at x, y in 0..4, the eye on +z; 800 x 600.
: TOP ( -- )
   s" top" T-LABEL
   0.5 0.0 0.0 -1.0 0 ROW!  0.0 0.5 0.0 -1.0 1 ROW!
   0.0 0.0 -0.0625 0.5 2 ROW!  AFFINE!  SHELF!
   800 600 VOLUME {: count:n px:r :}
   count 5 T=  px 0.009946609427258717 NEAR
   0.0 0.0 1.0 0.0 0 PLANE=
   0.8682431421244592 -0.4961389383568339 0.0 3.4920370965954692 1 PLANE=
   -0.8682431421244592 0.4961389383568339 0.0 4.465250445211504 2 PLANE=
   0.29620601710581057 0.5183605299351683 0.8022246296615702
   0.9044888059008803 3 PLANE=
   -0.29620601710581057 -0.5183605299351683 -0.8022246296615702
   4.162754994064795 4 PLANE= ;

\ A skewed affine map onto the tilted plane 0.1 x + 0.2 y + z = 0.5, the
\ box -20 -3 0.25 to 5 20 3.5; 1280 x 720.
: OBLIQUE ( -- )
   s" oblique" T-LABEL
   0.3 0.1 -0.05 0.2 0 ROW!  -0.08 0.15 0.25 -0.1 1 ROW!
   -0.1 0.2 0.12 0.4 2 ROW!  AFFINE!
   0.1 0.2 1.0 -0.5 PLANE!
   -20.0 -3.0 0.25 5.0 20.0 3.5 BOX!
   1280 720 VOLUME {: count:n px:r :}
   count 5 T=  px 0.01285273842045995 NEAR
   0.09759000867980991 0.19518001735961982 0.9759000722560548
   -0.4879500361280274 0 PLANE=
   0.992295296543169 -0.09428908832844912 -0.08037171318625887
   11.227254358784108 1 PLANE=
   -0.992295296543169 0.09428908832844912 0.08037171318625887
   5.224250819404626 2 PLANE=
   0.12385968791951427 0.7394060438273888 0.6617684489759311
   4.5299697776284695 3 PLANE=
   -0.12385968791951427 -0.7394060438273888 -0.6617684489759311
   9.15061629379561 4 PLANE= ;

\ Looking down at x in 100..102 and y in 0..2: that ground, grown by the
\ softbox, lies wholly outside the box's rectangle in key x.
: OUTSIDE ( -- )
   s" outside" T-LABEL
   1.0 0.0 0.0 -101.0 0 ROW!  0.0 1.0 0.0 -1.0 1 ROW!
   0.0 0.0 -0.0625 0.5 2 ROW!  AFFINE!  SHELF!
   800 600 VOLUME {: count:n px:r :}
   count 0 T=  px 0.0 NEAR ;

: KEPT ( -- ) 800 600 VOLUME {: count:n px:r :} ;

\ TOP's matrix with a fourth row 0 0.25 0 1, then with rows 0 and 1
\ parallel.
: REJECTED ( -- )
   s" rejected" T-LABEL
   0.5 0.0 0.0 -1.0 0 ROW!  0.0 0.5 0.0 -1.0 1 ROW!
   0.0 0.0 -0.0625 0.5 2 ROW!  0.0 0.25 0.0 1.0 3 ROW!  SHELF!
   [: KEPT ;] STUDIO:E-MATRIX TTHROWSQ
   1.0 0.0 0.0 -1.0 1 ROW!  AFFINE!
   [: KEPT ;] STUDIO:E-MATRIX TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   TOP
   OBLIQUE
   OUTSIDE
   REJECTED
   T-REPORT ;

;package

STUDIO-TEST:RUN
