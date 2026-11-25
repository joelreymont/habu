\ affine-test.f - rotations about a line and their composition with a
\ transform, against closed-form matrices.

require src/lib/test/assert.f
require src/lib/affine.f
require src/lib/fmath.f

package AFFINE-TEST

12 TYPED-BUFFER TM r
12 TYPED-BUFFER UM r

: TOL ( -- r ) 0.000000000001 ;

: NEAR ( r r -- ) f- fabs TOL f<= TTRUE ;

: NEAR3 ( r r r r r r -- )
   {: x:r y:r z:r u:r v:r w:r :}
   x u NEAR  y v NEAR  z w NEAR ;

: IDENTITY! ( ^r -- )
   {: m:^ :}
   12 0 ?do 0.0 m i cells + ! loop
   1.0 m !  1.0 m 5 cells + !  1.0 m 10 cells + ! ;

\ A transform that is neither orthogonal nor fixed at the origin.
: SKEWED! ( ^r -- )
   {: m:^ :}
   12 0 ?do i s>f 0.25 f* 0.5 f- m i cells + ! loop
   2.0 m ! ;

: ROW@ ( ^r n -- r r r r )
   {: m:^ row:n :}
   m row 4 * cells + {: p:^ :}
   p @  p cell+ @  p 2 cells + @  p 3 cells + @ ;

: QUARTER ( -- r ) FMATH:PI 0.5 f* ;

: RT3 ( -- r ) 1.0 3.0 fsqrt f/ ;

\ About +z through (1,0,0): (2,0,0) lands on (1,1,0), the axis stays put.
: QUARTER-Z ( -- )
   0 TM IDENTITY!
   0 TM 1.0 0.0 0.0 0.0 0.0 1.0 QUARTER AFFINE:ROTATE
   0 TM 2.0 0.0 0.0 AFFINE:MAP 1.0 1.0 0.0 NEAR3
   0 TM 1.0 0.0 5.0 AFFINE:MAP 1.0 0.0 5.0 NEAR3 ;

\ A third of a turn about (1,1,1) cycles the axes: x to y, y to z, z to x.
: THIRD-DIAGONAL ( -- )
   0 TM IDENTITY!
   0 TM 0.0 0.0 0.0 RT3 RT3 RT3
   FMATH:PI 2.0 f* 3.0 f/ AFFINE:ROTATE
   0 TM 0 ROW@ 0.0 NEAR 0.0 0.0 1.0 NEAR3
   0 TM 1 ROW@ 0.0 NEAR 1.0 0.0 0.0 NEAR3
   0 TM 2 ROW@ 0.0 NEAR 0.0 1.0 0.0 NEAR3 ;

\ A quarter turn about (2,3,6)/7 through (1,2,3): 49 A is
\ 4 -36 33 / 48 9 4 / -9 32 36, and 49 t is 18 20 -16.
: AT49 ( n n -- )
   {: idx:n want:n :}
   0 TM idx cells + @ want s>f 49.0 f/ NEAR ;

: SEVENTH ( n -- r ) s>f 7.0 f/ ;

: QUARTER-SKEW ( -- )
   0 TM IDENTITY!
   0 TM 1.0 2.0 3.0 2 SEVENTH 3 SEVENTH 6 SEVENTH
   QUARTER AFFINE:ROTATE
   0 4 AT49 1 -36 AT49 2 33 AT49 3 18 AT49
   4 48 AT49 5 9 AT49 6 4 AT49 7 20 AT49
   8 -9 AT49 9 32 AT49 10 36 AT49 11 -16 AT49 ;

\ The test's skew axis: through (1,2,3) along (1,2,2)/3.
: SKEW-TURN ( ^r r -- )
   {: m:^ angle:r :}
   m 1.0 2.0 3.0
   1.0 3.0 f/ 2.0 3.0 f/ 2.0 3.0 f/
   angle AFFINE:ROTATE ;

\ R∘T maps q where R maps T's image of q.
: COMPOSED ( -- )
   0 UM IDENTITY!  0 UM 0.7 SKEW-TURN
   0 TM SKEWED!
   0 TM 0.3 -1.2 2.5 AFFINE:MAP {: x:r y:r z:r :}
   0 TM 0.7 SKEW-TURN
   0 TM 0.3 -1.2 2.5 AFFINE:MAP
   0 UM x y z AFFINE:MAP NEAR3 ;

public

: RUN ( -- )
   T-RESET
   QUARTER-Z
   THIRD-DIAGONAL
   QUARTER-SKEW
   COMPOSED
   T-REPORT ;

;package

AFFINE-TEST:RUN
