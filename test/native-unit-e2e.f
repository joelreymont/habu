\ native-unit-e2e.f - a native build that imports the NBR package unit writes
\ the engine a cold build of the same tree writes, byte for byte.
\ The unit is test/native-unit-image.f's, exported from this tree; the
\ reference is test/whitebox-engine.f's keyed engine, tools/native-build.f run
\ with `whitebox` by the same donor in this tree. The import publishes NBR's
\ code, dictionary rows and checker records without compiling its source, then
\ compiles every later file, src/arch/arm64/passes.f's NBR clients among them,
\ against those records. Equal bytes therefore cover the imported code and the
\ checker state the engine captured: an import that dropped or loosened a
\ record of NBR's fails here. test/native-unit-lib.f has the fixture and the
\ other row; run alone: bin/hb --load test/native-unit-e2e.f

require lib/test.f
require lib/string.f
require tools/chain-run.f
require test/whitebox-engine.f
require test/native-unit-lib.f

package NATIVE-UNIT-TEST

: IMPORT-PARITY ( -- )
   s" an NBR unit import writes this tree's cold unsealed engine" T-LABEL
   WHITEBOX-ENGINE:PATH$ {: cold:ptr coldu:n :}
   s" hb-import" SOURCE-ROOT:CWD$ IMPORT
   SUCCESS
   OUT$ s" native-build: NBR unit hit" CONTAINS? TTRUE
   cold coldu s" hb-import" AT CHAIN-RUN:SAME-FILES? TTRUE ;

public

: MAIN ( -- )
   T-RESET
   s" native-unit-e2e" SETUP
   IMPORT-PARITY
   T-REPORT
   s" native unit tree: " type ROOT$ type cr ;

;package

NATIVE-UNIT-TEST:MAIN
