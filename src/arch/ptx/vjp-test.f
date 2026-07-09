\ vjp-test.f - per-entry unit tests for the VJP: table (src/arch/ptx/vjp.f).
\
\ Every entry's adjoint expansion and saves count is asserted exactly; the
\ review-corrected direction facts get explicit negatives: OVER's adjoint SUMS
\ the copied value's two cotangents (never the permutation model that silently
\ drops a gradient term) and DROP's adjoint is a typed zero (never a leaked
\ cotangent). Device gradcheck coverage per entry lives in the ad-entry
\ device suite (tools/ptx/ad-entry-lib.f + tools/ptx/zed-gradcheck-suite.f);
\ LOAD/STORE scatter-add adjoints are device-proven by the committed
\ scatter-add/redadd Orin tools.

require lib/ptx/test-prelude.f

T-RESET

\ ---- elementwise ----------------------------------------------------------------
s" +." VJP-ADJOINT$ s" DUP" T$=
s" +." VJP-SAVES# 0 T=
s" -." VJP-ADJOINT$ s" DUP NEG" T$=
s" -." VJP-SAVES# 0 T=
s" *." VJP-ADJOINT$ s" DUP SAVED-Y *. SWAP SAVED-X *." T$=
s" *." VJP-SAVES# 2 T=
s" /." VJP-ADJOINT$ s" DUP SAVED-Y /. SWAP SAVED-Z *. SAVED-Y /. NEG" T$=
s" /." VJP-SAVES# 2 T=

\ ---- broadcast and fused --------------------------------------------------------
s" SCALE" VJP-ADJOINT$ s" DUP SAVED-A SCALE SWAP SAVED-X *. BLOCK-SUM" T$=
s" SCALE" VJP-SAVES# 2 T=
s" FMA." VJP-ADJOINT$ s" DUP DUP SAVED-X *. BLOCK-SUM ROT SAVED-A SCALE ROT" T$=
s" FMA." VJP-SAVES# 2 T=
s" PTX:B-" VJP-ADJOINT$ s" DUP BLOCK-SUM NEG" T$=
s" PTX:B-" VJP-SAVES# 0 T=
s" PTX:B/" VJP-ADJOINT$ s" DUP SAVED-S PTX:B/ SWAP SAVED-Z *. BLOCK-SUM NEG SAVED-S PTX:U/" T$=
s" PTX:B/" VJP-SAVES# 2 T=

\ ---- unary and collectives ------------------------------------------------------
s" EXP." VJP-ADJOINT$ s" SAVED-Y *." T$=
s" EXP." VJP-SAVES# 1 T=
s" BLOCK-SUM" VJP-ADJOINT$ s" BROADCAST" T$=
s" BROADCAST" VJP-ADJOINT$ s" BLOCK-SUM" T$=
s" BLOCK-MAX" VJP-ADJOINT$ s" SAVED-X SAVED-MX BLOCK-MAX-SELECT" T$=
s" BLOCK-MAX" VJP-SAVES# 2 T=
s" NEG" VJP-ADJOINT$ s" NEG" T$=

\ ---- stack and structural (the review-corrected entries) --------------------------
s" DUP" VJP-ADJOINT$ s" +." T$=
\ OVER copies a ( a b -- a b a ): its adjoint SUMS da1+da2 - NOT a permutation
s" OVER" VJP-ADJOINT$ s" ROT +. SWAP" T$=
s" OVER" VJP-ADJOINT$ s" ROT SWAP" T$<>
s" OVER" VJP-SAVES# 0 T=
\ DROP's adjoint is a typed zero - never the incoming cotangent
s" DROP" VJP-ADJOINT$ s" ZERO." T$=
s" DROP" VJP-SAVES# 0 T=
\ genuine permutations: SWAP self-inverse; ROT's inverse is ROT ROT
s" SWAP" VJP-ADJOINT$ s" SWAP" T$=
s" ROT" VJP-ADJOINT$ s" ROT ROT" T$=

\ ---- memory ----------------------------------------------------------------------
s" LOAD" VJP-ADJOINT$ s" SCATTER-ADD" T$=
s" STORE" VJP-ADJOINT$ s" LOAD" T$=
s" LOAD-ONCE" VJP-ADJOINT$ s" STORE-ONCE" T$=
s" STORE-ONCE" VJP-ADJOINT$ s" LOAD-ONCE" T$=
s" ROW-LOAD" VJP-ADJOINT$ s" ROW-SCATTER-ADD" T$=
s" ROW-STORE" VJP-ADJOINT$ s" ROW-LOAD" T$=
s" ROW-LOAD-ONCE" VJP-ADJOINT$ s" ROW-STORE-ONCE" T$=
s" ROW-STORE-ONCE" VJP-ADJOINT$ s" ROW-LOAD-ONCE" T$=

\ ---- registry contracts ------------------------------------------------------------
VJP-N @ 26 T=
: VJPT-MISS ( -- )  s" NO-SUCH-OP" VJP-ADJOINT$ 2drop ;
' VJPT-MISS E-PTX-NOVJP TTHROWS
: VJPT-DUP-REG ( -- )  s" +." 0 s" DUP" VJP-REGISTER ;
' VJPT-DUP-REG E-PTX-SYNTAX TTHROWS
\ ZERO. and SAVED-A are table TOKENS, not words: the ad-gen lowering resolves
\ them (EMIT-ZERO / recompute binding); see lib/ptx/ad-gen-test.f.

T-REPORT
