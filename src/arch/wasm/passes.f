\ passes.f - WPASS, the Wasm backend's rows in src/compiler/native/backend.f's
\ pass table, registered by WBACK:INSTALL. No Wasm routine is placed in the
\ engine's code region: NCOMP runs these rows as a shadow's
\ (src/compiler/native/shadow.f) for an open Wasm binding, in a context nested in
\ the definition's, ending in EMIT-UNPLACED (compiler.f SHADOW-WORK).

require src/lib/prelude.f
require src/lib/errors.f
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

: ARCH ( -- CTARGET:arch )
   CTARGET-ARCH:WASM ;

\ FIXPOINT hands back the module selection froze: Wasm has no registers to
\ allocate or spills to lower, and rebuilding renumbers values.
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
      i WENC:CALL-IMPL@  i WENC:CALL-LOC@
      NEMIT:CALL-SITE+
   loop
   WENC:ADDR-SITES 0 ?do
      i WENC:ADDR-SITE@  i WENC:ADDR-SITE-KIND@  -1 NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

: EMIT-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- )
   nip [: WSEL:ARITY ;] WENC:ENCODE
   ROWS ;

\ The rows answer until RETIRE runs, which the driver does on the accepting and
\ the refusing path alike, so NEMIT is empty for the engine's own emission.
: RETIRE ( -- )
   WENC:RETIRE
   NEMIT:CLEAR ;

\ RELEASE, FORGET and PREPARE: WSEL's builder and modules belong to the nested
\ context, which takes them when it leaves; WSTRUCT keeps no session prototype;
\ WSEL, WCTL and WENC keep no scratch count beside their dynamic buffers, which
\ reserve again after an image capture releases them.
: NOTHING-HELD ( -- ) ;

: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;

public

\ In NBACK-PASS's field order. SELECT freezes a fresh WSTRUCT builder with
\ WSTRUCT:FREEZE, the full IR-BUILD:FREEZE (src/arch/wasm/wstruct.f).
: PASS ( -- NBACK:pass )
   [: WSEL:DECLARE ;] [: WSEL:SELECT ;] [: AS-SELECTED ;]
   [: PLACED-UNSUPPORTED ;] [: EMIT-UNPLACED ;] [: NOTHING-HELD ;] [: RETIRE ;]
   [: NO-PROTOTYPE ;] [: NOTHING-HELD ;] [: NOTHING-HELD ;]
      NBACK-PASS:MAKE ;

;package
