\ affine.f - binary64 3D affine transforms: rotate one about a line, map a
\ point through one.
\ A transform is twelve binary64 cells, row-major 3x4,
\ a00 a01 a02 t0 a10 a11 a12 t1 a20 a21 a22 t2, mapping p to A p + t.
\ ROTATE's R is Rodrigues' matrix cos I + sin [d]x + (1 - cos) d d^T.
\ STORAGE CLASS. CALLER-OWNED: the transform lives in the caller's memory; the
\ module keeps no state.

require src/lib/fmath.f

package AFFINE

private

\ The cell n down a column from its top, rows four cells apart.
: DOWN ( ptr r n -- ptr r ) 4 * cells + ;

\ The row a b c of a matrix times the column whose top is at col.
: ROW*COLUMN ( ptr r r r r -- r )
   {: col:ptr a:r b:r c:r :}
   col @ a f*
   col 1 DOWN @ b f* f+
   col 2 DOWN @ c f* f+ ;

: COLUMN! ( r r r ptr r -- )
   {: x:r y:r z:r col:ptr :}
   x col !
   y col 1 DOWN !
   z col 2 DOWN ! ;

\ Adds x y z to the translation column t.
: SHIFT ( ptr r r r r -- )
   {: m:ptr x:r y:r z:r :}
   m 3 cells + {: t:ptr :}
   t @ x f+  t 1 DOWN @ y f+  t 2 DOWN @ z f+  t COLUMN! ;

\ The Rodrigues entry k a b + w: a diagonal one takes w = cos, an off-diagonal
\ one the sine term of [d]x.
: TERM ( r r r r -- r )
   {: a:r b:r k:r w:r :}
   a b f* k f* w f+ ;

\ Rodrigues' matrix, row-major, for the unit direction dx dy dz and the angle.
: RODRIGUES ( r r r r -- r r r r r r r r r )
   {: dx:r dy:r dz:r angle:r :}
   angle FMATH:COS {: c:r :}
   angle FMATH:SIN {: s:r :}
   1.0 c f- {: k:r :}
   s dx f* {: sx:r :}
   s dy f* {: sy:r :}
   s dz f* {: sz:r :}
   dx dx k c TERM  dx dy k sz fnegate TERM  dx dz k sy TERM
   dy dx k sz TERM  dy dy k c TERM  dy dz k sx fnegate TERM
   dz dx k sy fnegate TERM  dz dy k sx TERM  dz dz k c TERM ;

\ One row of A p + t, the row's first cell at row.
: ROW-MAP ( ptr r r r r -- r )
   {: row:ptr x:r y:r z:r :}
   row @ x f*
   row cell+ @ y f* f+
   row 2 cells + @ z f* f+
   row 3 cells + @ f+ ;

public

\ Replaces the transform T at m by R∘T, R the right-handed rotation by angle
\ radians about the line through px py pz with direction dx dy dz, whose unit
\ length is the caller's contract: A becomes R A and t becomes
\ R (t - p) + p. An angle outside FMATH:SIN's domain is FMATH:E-DOMAIN,
\ with m unchanged.
: ROTATE ( ptr r r r r r r r r -- )
   {: m:ptr px:r py:r pz:r dx:r dy:r dz:r angle:r :}
   dx dy dz angle RODRIGUES
   {: r00:r r01:r r02:r r10:r r11:r r12:r r20:r r21:r r22:r :}
   m px fnegate py fnegate pz fnegate SHIFT
   4 0 ?do
      m i cells + {: col:ptr :}
      col r00 r01 r02 ROW*COLUMN
      col r10 r11 r12 ROW*COLUMN
      col r20 r21 r22 ROW*COLUMN
      col COLUMN!
   loop
   m px py pz SHIFT ;

\ The point x y z under the transform at m, A p + t.
: MAP ( ptr r r r r -- r r r )
   {: m:ptr x:r y:r z:r :}
   m x y z ROW-MAP
   m 4 cells + x y z ROW-MAP
   m 8 cells + x y z ROW-MAP ;

;package
