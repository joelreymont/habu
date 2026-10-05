\ passes.f - WPASS, the Wasm backend's rows in the compiler's pass table
\ (src/compiler/native/backend.f), which WBACK:INSTALL registers
\ (src/arch/wasm/backend.f).
\
\ WHAT RUNS. No Wasm routine is ever placed in the engine's code region, so the
\ rows are reached as a shadow's (src/compiler/native/shadow.f): NCOMP runs them
\ for an open Wasm binding, in a context nested in the definition's, from the
\ module NBACK:FREEZE froze, and ends in EMIT-UNPLACED (src/compiler/native/
\ compiler.f SHADOW-WORK). DECLARE and SELECT are WSEL's: SELECT selects into a
\ fresh WSTRUCT builder and freezes it with WSTRUCT:FREEZE, the full
\ IR-BUILD:FREEZE (docs/wasm-backend.md 17.2). PRUNE and the lowering FIXPOINT
\ hand that module back as it is: Wasm has no registers to allocate and no
\ spills to lower, and rebuilding renumbers values. EMIT is refused, as there
\ is no placed Wasm emission.
\
\ THE EMISSION. EMIT-UNPLACED encodes the frozen module through WENC, each
\ function's lanes WSEL's ARITY, and ROWS states the sealed emission in NEMIT's
\ producer rows, all in that one word, to be rebased when the backend consumer
\ boundary (habu-give-each-backend-b6f7ea4f) replaces them. RETIRE gives the
\ encoder's emission back and clears NEMIT; the driver runs it on the accepting
\ and the refusing path alike, so NEMIT is empty for the engine's own emission.
\
\ NOTHING ELSE IS HELD. WSEL's builder and modules are the nested context's,
\ which takes them when it leaves; WSTRUCT keeps no session prototype, each
\ module interning its own spellings; and WSEL, WCTL and WENC keep no scratch
\ count beside their dynamic buffers, which reserve again on use after an
\ image capture releases them.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
require src/compiler/native/emission.f
require src/arch/wasm/select.f
require src/arch/wasm/encode.f

package WPASS
private

\ The machine every emission these rows seal is for.
: ARCH ( -- CTARGET:arch )
   CTARGET-ARCH:WASM ;

\ PRUNE and FIXPOINT: the module selection froze, untouched.
: AS-SELECTED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   nip ;

\ ---- emission ----------------------------------------------------------------
\ The placed row: no Wasm routine has a slot in the engine's code region, so it
\ is refused as a stage this backend does not have, as the ARM64 row refuses an
\ unplaced emission (src/arch/arm64/passes.f UNPLACED-UNSUPPORTED).
: PLACED-UNSUPPORTED ( IR-CTX:ctx IR-BUILD:module n -- )
   E-CTGT-UNLOADED throw ;

\ The sealed emission as NEMIT's rows. WENC's readers are shaped like the rows,
\ so each is copied across and nothing is decoded. No trailing return is split
\ off: a body's `end` is inside it, and the emission is the whole record.
: ROWS ( -- )
   WENC:BYTES WENC:SIZE 0 ARCH NEMIT:OPEN
   WENC:FUNS 0 ?do
      i WENC:FUNCTION-OFFSET@ NEMIT:FUNCTION+
   loop
   WENC:CALL-SITES 0 ?do
      i WENC:CALL-SITE@  i WENC:CALL-KIND@  i WENC:CALL-TARGET@
      NEMIT:CALL-SITE+
   loop
   WENC:ADDR-SITES 0 ?do
      i WENC:ADDR-SITE@  i WENC:ADDR-SITE-KIND@  NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

: EMIT-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- )
   nip [: WSEL:ARITY ;] WENC:ENCODE
   ROWS ;

\ The rows answer until this row runs, on the accepting and the refusing path.
: RETIRE ( -- )
   WENC:RETIRE
   NEMIT:CLEAR ;

\ RELEASE, FORGET and PREPARE: nothing is held (see the header).
: NOTHING-HELD ( -- ) ;

: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;

public

\ The rows WBACK:INSTALL registers, in NBACK-PASS's field order.
: PASS ( -- NBACK:pass )
   [: WSEL:DECLARE ;] [: WSEL:SELECT ;] [: AS-SELECTED ;] [: AS-SELECTED ;]
   [: PLACED-UNSUPPORTED ;] [: EMIT-UNPLACED ;] [: NOTHING-HELD ;] [: RETIRE ;]
   [: NO-PROTOTYPE ;] [: NOTHING-HELD ;] [: NOTHING-HELD ;]
      NBACK-PASS:MAKE ;

;package
