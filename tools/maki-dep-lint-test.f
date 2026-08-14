\ maki-dep-lint-test.f - checked fixtures for the one-way habu<-maki dependency lint.
\ Load after lib/test.f and tools/maki-dep-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/maki-dep-lint-core.f

\ The fixtures live in a package of their own, as docs/forth.md § Testing asks:
\ the MDLT- prefix every name used to carry is what a package scope is for.
package MAKI-DEP-LINT-TEST

private

: EXT ( -- )
   s" lib/ptx/cg.f"  MAKI-DEP-LINT:SRC? TTRUE
   s" test/run.fs"   MAKI-DEP-LINT:SRC? TTRUE
   s" docs/forth.md" MAKI-DEP-LINT:SRC? TFALSE
   s" maki/array.f"  MAKI-DEP-LINT:SRC? TTRUE ;

: DETECT ( -- )
   \ a bare load token referencing maki/ is a finding
   s" --load maki/array.f"        MAKI-DEP-LINT:COUNT 1 T=
   \ a string-literal load path (TOKENIZE keeps string bodies) is caught too
   s" maki/eval/eval.f maki/optim.f"    MAKI-DEP-LINT:COUNT 2 T= ;

: NO-FALSE-POSITIVE ( -- )
   \ `\` line comments are stripped -> mentioning maki/ in prose is NOT a finding
   s" \ this loads maki/array.f at the maki layer" MAKI-DEP-LINT:COUNT 0 T=
   \ `( )` stack-effect comments are stripped too
   s" : F ( maki/x -- n ) dup ;"   MAKI-DEP-LINT:COUNT 0 T=
   \ clean core code never trips
   s" : SQUARE ( n -- n ) dup * ;" MAKI-DEP-LINT:COUNT 0 T= ;

: GATE-ROUTE ( -- )
   \ dot habu-route-the-maki-e61d8a1b: the gate harness may name the maki suite
   \ entry it spawns. Exactly that token, in exactly test/run-lib.f, is allowed;
   \ the s" quote form is proven end-to-end by LIVE-LINT scanning the real file.
   s" test/run-lib.f" MAKI-DEP-LINT:PATH!
   s" --load maki/test.f"     MAKI-DEP-LINT:COUNT 0 T=   \ sanctioned bare token: allowed
   s" --load maki/report.f"   MAKI-DEP-LINT:COUNT 1 T=   \ any other maki/ token here still fails
   s" --load maki/test.fs"    MAKI-DEP-LINT:COUNT 1 T=   \ 12-char near-miss is not the quote form
   s" test/other.f" MAKI-DEP-LINT:PATH!
   s" --load maki/test.f"     MAKI-DEP-LINT:COUNT 1 T=   \ sanctioned token in another file still fails
   0 MAKI-DEP-LINT:PATHU ! ;                             \ reset so later scans are file-agnostic

: LIVE-LINT ( -- )
   \ the real tree (src/ lib/ test/) must be maki-free -> the walk returns clean
   MAKI-DEP-LINT:RUN ;

: MAIN ( -- )
   T-RESET
   EXT
   DETECT
   NO-FALSE-POSITIVE
   GATE-ROUTE
   LIVE-LINT
   T-REPORT ;

public

EXPORT MAIN

;package

MAKI-DEP-LINT-TEST:MAIN
