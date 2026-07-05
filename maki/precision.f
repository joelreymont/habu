\ maki/precision.f - gate-licensed precision classes + per-class tolerance rows.
\
\ CAD-PLAN 8.1 lever 5 ("precision policy LICENSED by the gates") + section 11.
\ Precision is NEVER a global flag: PREC! records a REQUESTED precision for one
\ op-registry region class, maki/lower-golden.f judges that class's golden under
\ the requested precision's tolerance row, and the LICENSE is the passing golden
\ verdict itself. The default is PREC-F32 for every class; PREC-RESET restores it.
\
\ Tolerance rows are (atol, rtol) pairs stored as mantissa*10^exp integers
\ (POW10 renders the floats; no float memory needed):
\   f32 (the lower-golden slice 1-4 derivations, CAD-PLAN section 11):
\     elementwise 1e-6/1e-5, row-reduce 1e-6/1e-4, matmul 1e-6/1e-4,
\     movement 1e-6/1e-6 (NUM-EXACT copy).
\   tf32 (matmul class ONLY): atol 1e-6, rtol 2e-3. Derivation: TF32 keeps a
\     10-bit mantissa (unit roundoff 2^-11 ~= 4.9e-4 per rounded product); the
\     measured end-to-end TF32 GEMM error on this device class is rel_err
\     7.5e-4..7.9e-4 vs an f32 reference at K=512..2048 (docs/eval-triton.md
\     2026-07-04 GEMM section, Triton tl.dot on sm_87 tensor cores). rtol 2e-3
\     keeps ~2.5x headroom over the measured worst case; atol stays at the f32
\     floor because the accumulator is still f32.
\ A (class, precision) pair with NO row is UNLICENSED: PREC! refuses it
\ (E-PREC-ROW). fp16/bf16 rows land only with a measured derivation (the MMA
\ lane); the decode class has no golden lowering yet, so it carries no row.
\
\ Fail closed: precision id / class id out of range and unlicensed pairs are
\ named throws. maki -> habu only; precision owns -5141..-5143.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-registry.f

-5141 constant E-PREC-ID     \ precision id out of range
-5142 constant E-PREC-CLASS  \ region class id out of range
-5143 constant E-PREC-ROW    \ unlicensed (class, precision) pair: no tolerance row

package MAKI
public

\ ---- precision class ids -----------------------------------------------------
0 constant PREC-F32            \ full f32 fma.rn (the default everywhere)
1 constant PREC-TF32           \ TF32 tensor-core input rounding, f32 accumulate
2 constant PREC-N              \ range bound

: PREC-NAME ( n -- ptr u8 n )
   case
      PREC-F32  of s" f32"  endof
      PREC-TF32 of s" tf32" endof
      E-PREC-ID throw
   endcase ;

: PREC-MAX ( n n -- n ) {: a:n b:n :}  a b < if b else a then ;

private

: PREC-CK ( n -- ) {: p:n :}
   p 0 <  p PREC-N >=  or if E-PREC-ID throw then ;
: PREC-CK-CLASS ( n -- ) {: c:n :}
   c 0 <  c CLASS-N >=  or if E-PREC-CLASS throw then ;

\ ---- tolerance rows: (atol-mant, atol-exp, rtol-mant, rtol-exp) per (class, prec)
\ rtol-mant 0 marks an UNLICENSED pair (no measured derivation exists yet).
4 constant PREC-ROW-CELLS
create PREC-ROWS CLASS-N PREC-N * PREC-ROW-CELLS * cells allot

: PREC-ROW-AT ( n n -- ptr a ) {: cls:n prec:n :}
   cls PREC-CK-CLASS  prec PREC-CK
   cls PREC-N * prec +  PREC-ROW-CELLS * cells  PREC-ROWS + ;

: PREC-ROW! ( n n n n n n -- ) {: cls:n prec:n am:n ae:n rm:n re:n :}
   cls prec PREC-ROW-AT {: a:ptr :}
   am a !  ae a 1 cells + !  rm a 2 cells + !  re a 3 cells + ! ;

: PREC-ROWS-INIT ( -- )
   CLASS-N 0 ?do
      PREC-N 0 ?do  j i 0 0 0 0 PREC-ROW!  loop        \ default: unlicensed
   loop
   CLASS-EW         PREC-F32  1 -6 1 -5 PREC-ROW!
   CLASS-ROW-REDUCE PREC-F32  1 -6 1 -4 PREC-ROW!
   CLASS-MATMUL     PREC-F32  1 -6 1 -4 PREC-ROW!
   CLASS-MOVEMENT   PREC-F32  1 -6 1 -6 PREC-ROW!
   CLASS-MATMUL     PREC-TF32 1 -6 2 -3 PREC-ROW! ;    \ measured ~8e-4, ~2.5x headroom
PREC-ROWS-INIT

\ ---- requested precision per class (default f32; PREC-RESET restores) ---------
create PREC-REQ CLASS-N cells allot

public

: PREC-LICENSED? ( n n -- bool ) {: cls:n prec:n :}   \ does a tolerance row exist?
   cls prec PREC-ROW-AT 2 cells + @ 0<> ;

: PREC-RESET ( -- )  CLASS-N 0 ?do  PREC-F32 i cells PREC-REQ + !  loop ;

\ PREC! requests a precision for a region class. The pair must carry a tolerance
\ row (else E-PREC-ROW: unlicensed); the golden gate then judges under that row -
\ the passing verdict IS the license, never this request alone.
: PREC! ( n n -- ) {: prec:n cls:n :}
   cls prec PREC-LICENSED? 0= if E-PREC-ROW throw then
   prec  cls cells PREC-REQ + ! ;

: PREC@ ( n -- n ) {: cls:n :}
   cls PREC-CK-CLASS  cls cells PREC-REQ + @ ;

private
: PREC-F ( n n -- r ) {: m:n e:n :}  m s>f  e POW10  f* ;
: PREC-ACTIVE-ROW ( n -- ptr a ) {: cls:n :}       \ row of the class's ACTIVE precision
   cls dup PREC@ PREC-ROW-AT {: a:ptr :}
   a 2 cells + @ 0= if E-PREC-ROW throw then      \ the active pair must be licensed
   a ;
public

: PREC-ATOL ( n -- r ) {: cls:n :}
   cls PREC-ACTIVE-ROW {: a:ptr :}  a @  a 1 cells + @  PREC-F ;
: PREC-RTOL ( n -- r ) {: cls:n :}
   cls PREC-ACTIVE-ROW {: a:ptr :}  a 2 cells + @  a 3 cells + @  PREC-F ;

PREC-RESET

end-package
