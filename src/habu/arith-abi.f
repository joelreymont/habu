\ arith-abi.f - the arithmetic primitives' refusal ABI, shared by the
\ source-loaded and recovered emitters. Its own file as stack-abi.f is: the
\ build-fixpoint payload carries the emitters without the prefix holding prims.f.
\ errors.f owns the code as E-DIV-ZERO; the emitters (habu1.f BDIV0?) and the
\ Gforth mirror compile before src/lib exists, so the pair is re-registered here,
\ the one form tools/error-code-lint.f admits; test/prim-parity.f keeps them equal.
package ARITH-ABI
public

\ Division is the one partial arithmetic: `+`, `-` and `*` wrap, while `/`, `mod`
\ and `/mod` test the divisor and throw this code instead of letting arm64's SDIV
\ answer zero or trapping the process. The divisor came from the program's own
\ arithmetic, so a caller can always catch it. MIN-N -1 / is not a second refusal:
\ it wraps to MIN-N; docs/forth.md states it and test/prim-parity.f pins it.
-6400 constant E-DIV-ZERO

;package
