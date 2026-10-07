\ native-emit-run-child.f - the ARM64 emission cases that run the bytes.
\
\ test/compiler/native-emit.f compares the emitted words; this file emits the
\ same shapes, built by test/compiler/native-emit-shapes.f, with the emitter
\ loaded from the same sources inside the whitebox window,
\ publishes them into the engine's code space and calls them, comparing the
\ source-level arithmetic's answer. Publishing the bytes (code-publish, package
\ NPUB) and calling them (the bounded FFI call, package FFI) are owner rows a
\ checked caller outside their owners is refused, so
\ test/compiler/native-run-fixture.f reaches them through the public words
\ test/mcode-window-prepare.f opens before the seal. That makes this a window
\ child: native-emit.f's RUN-CHILD-CASE has
\ test/native-window-owner-child.f load it after test/mcode-window-prepare.f,
\ on the unsealed engine test/whitebox-child.f names. It prints `test: ok`, then
\ the window `window: 0`.

s" src/core/checker-owner-abi.f" provided
s" src/core/engine-error.f" provided
require lib/test.f
require test/compiler/native-emit-shapes.f
require test/compiler/native-run-fixture.f

package A64EMIT-TEST
private

\ ---- publishing and calling the emitted bytes --------------------------------
\ The store into code space and the C-ABI call are the two engine boundaries, and
\ they live in test/compiler/native-run-fixture.f so the comparison harness runs
\ the emitted bytes exactly the way this suite does.
: PUBLISH ( -- n )       NRUN:PUBLISH ;
: EXEC0 ( n -- n )       NRUN:EXEC0 ;
: EXEC1 ( n n -- n )     NRUN:EXEC1 ;
: EXEC2 ( n n n -- n )   NRUN:EXEC2 ;
: EXEC3 ( n n n n -- n ) NRUN:EXEC3 ;

\ ---- the emitted bytes, executed ---------------------------------------------
\ Published into the engine's own code space and called as a leaf routine. The
\ answer is the source-level arithmetic's, not the emitter's idea of it.
: RUN-SQUARE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   RESULT-REG
   7 PUBLISH EXEC1 ;

: RUN-SQUARE-CASE ( -- )
   s" the emitted square really squares when the machine runs it" T-LABEL
   WBND [: RUN-SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   49 T= 0 T= ;

: RUN-DIFF-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-DIFF
   4 EMITTED
   RESULT-REG
   9 4 PUBLISH EXEC2 ;

: RUN-DIFF-CASE ( -- )
   s" the emitted difference subtracts the second argument from the first" T-LABEL
   WBND [: RUN-DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 0 T= ;

\ The emitted division computes what the engine computes, truncating toward zero
\ rather than flooring: -7 over 2 is -3 and not -4. The two negative cases are
\ what say the rounding of a compiled division is the rounding of an interpreted
\ one.
: RUN-DIV-BODY ( IR-CTX:ctx -- n n n )
   HIR-MOD
   BUILD-DIV
   NRUN:PLACE
   4 EMITTED
   PUBLISH {: fn:n :}
   7 2 fn EXEC2
   -7 2 fn EXEC2
   7 -2 fn EXEC2 ;

: RUN-DIV-CASE ( -- )
   s" the emitted division truncates toward zero, as the engine's does" T-LABEL
   WBND [: RUN-DIV-BODY ;] IR-CTX:WITH-CONTEXT
   -3 T= -3 T= 3 T= ;

: RUN-SUM3-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-SUM3
   4 EMITTED
   RESULT-REG
   1 2 3 PUBLISH EXEC3 ;

: RUN-SUM3-CASE ( -- )
   s" the emitted three-argument sum really adds all three" T-LABEL
   WBND [: RUN-SUM3-BODY ;] IR-CTX:WITH-CONTEXT
   6 T= 0 T= ;

: RUN-REUSE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-REUSE
   4 EMITTED
   RESULT-REG
   10 3 PUBLISH EXEC2 ;

: RUN-REUSE-CASE ( -- )
   s" the emitted reuse shape adds the first argument in twice" T-LABEL
   WBND [: RUN-REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   23 T= 0 T= ;

: RUN-WIDE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   RESULT-REG
   PUBLISH EXEC0 ;

: RUN-WIDE-CASE ( -- )
   s" the emitted move-wide chain materialises the whole literal" T-LABEL
   WBND [: RUN-WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   $1234000000005678 T= 0 T= ;

\ The Habu-convention square of native-emit.f's SQUARE-HABU-CASE, entered over
\ the data stack: its cell under the pointer goes in as seven and comes back as
\ forty-nine.
: RUN-SQUARE-HABU-BODY ( IR-CTX:ctx -- n )
   HIR-MOD
   BUILD-SQUARE
   4 1 1 EMITTED-HABU
   7 PUBLISH NRUN:ENTER1 ;

: RUN-SQUARE-HABU-CASE ( -- )
   s" a cell under the pointer runs as the square" T-LABEL
   WBND [: RUN-SQUARE-HABU-BODY ;] IR-CTX:WITH-CONTEXT
   49 T= ;

: RUN-SPILL-BODY ( IR-CTX:ctx -- n n )
   SPILL-EMITTED
   NFIX:RESULT-REG
   PUBLISH EXEC0 ;

: RUN-SPILL-CASE ( -- )
   s" the emitted spilled program computes what its values add up to" T-LABEL
   WBND [: RUN-SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   $11 $22 + $33 + $44 + $55 + 2 * T= 0 T= ;

: RUN-REMAT-BODY ( IR-CTX:ctx -- n n )
   REMAT-EMITTED
   NFIX:RESULT-REG
   PUBLISH EXEC0 ;

: RUN-REMAT-CASE ( -- )
   s" the emitted re-emitting program computes what its constants add up to"
   T-LABEL
   WBND [: RUN-REMAT-BODY ;] IR-CTX:WITH-CONTEXT
   $11 $22 + $33 + $44 + $55 + T= 0 T= ;

: RUN-SECOND-BODY ( IR-CTX:ctx -- n n )
   SECOND-EMITTED
   NFIX:RESULT-REG
   7 9 PUBLISH EXEC2 ;

: RUN-SECOND-CASE ( -- )
   s" the emitted copy really returns the second argument" T-LABEL
   WBND [: RUN-SECOND-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 0 T= ;

public

: RUN ( -- )
   T-RESET
   RUN-SQUARE-HABU-CASE
   RUN-SQUARE-CASE
   RUN-DIFF-CASE
   RUN-DIV-CASE
   RUN-SUM3-CASE
   RUN-REUSE-CASE
   RUN-WIDE-CASE
   RUN-SPILL-CASE
   RUN-REMAT-CASE
   RUN-SECOND-CASE
   T-REPORT ;

;package

A64EMIT-TEST:RUN
