\ The arithmetic primitives' refusal ABI, shared by the source-loaded and
\ recovered emitters.
\
\ DIVISION IS THE ONE PARTIAL ARITHMETIC. `+`, `-` and `*` wrap: every pair of
\ cells has an answer. `/`, `mod` and `/mod` do not — no cell is the quotient by
\ zero — so their bodies test the divisor and throw this code instead of letting
\ arm64's SDIV answer zero or trapping the process. It is a CALLER error the
\ program can fix and recover from: the divisor came from the program's own
\ arithmetic, so there is always a caller to hand the throw to.
\
\ lib/errors.f owns the code as E-DIV-ZERO; the engine emitters
\ (src/habu/habu1.f BDIV0?) and the Gforth recovery mirror compile before any
\ lib/ file exists, so the same (code, name) pair is re-registered here -- the
\ one form tools/error-code-lint.f admits -- and test/prim-parity.f keeps the
\ two spellings equal by catching the primitive's throw and comparing it with
\ lib/errors.f's constant.
\
\ IT IS ITS OWN FILE for the reason src/habu/stack-abi.f is: the builder payload
\ tools/build-fixpoint.f assembles carries the emitters WITHOUT the checker boot
\ prefix that holds src/habu/prims.f, so a constant the emitters need has to sit
\ in a file that payload can carry on its own.
\
\ MIN-N -1 / IS NOT A SECOND REFUSAL. Its quotient 2^63 has no cell, so it wraps
\ to MIN-N the way every other overflowing operation wraps; docs/forth.md states
\ that contract and test/prim-parity.f pins it.
package ARITH-ABI
public

-6400 constant E-DIV-ZERO

;package
