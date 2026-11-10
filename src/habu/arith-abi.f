\ arith-abi.f - the arithmetic primitives' refusal ABI, shared by the emitters
\ (habu1.f BDIV0?, kernel-x64.f, src/compiler/native/emit-x64.f). error.f owns
\ the code as E-DIV-ZERO; the pair is re-registered here, the one form
\ tools/error-code-lint.f admits; test/prim-parity.f keeps them equal.
package ARITH-ABI
public

\ Division is the one partial arithmetic: `+`, `-` and `*` wrap, while `/`, `mod`
\ and `/mod` test the divisor and throw this code instead of letting arm64's SDIV
\ answer zero or trapping the process. The divisor came from the program's own
\ arithmetic, so a caller can always catch it. MIN-N -1 / is not a second refusal:
\ it wraps to MIN-N; docs/forth.md states it and test/prim-parity.f pins it.
-6400 constant E-DIV-ZERO

;package
