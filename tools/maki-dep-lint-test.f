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

: MDLT-EXT ( -- )
   s" lib/ptx/cg.f"  MDL-SRC? TTRUE
   s" test/run.fs"   MDL-SRC? TTRUE
   s" docs/forth.md" MDL-SRC? TFALSE
   s" maki/array.f"  MDL-SRC? TTRUE ;

: MDLT-DETECT ( -- )
   \ a bare load token referencing maki/ is a finding
   s" --load maki/array.f"        MDL-COUNT 1 T=
   \ a string-literal load path (TOKENIZE keeps string bodies) is caught too
   s" maki/eval.f maki/optim.f"    MDL-COUNT 2 T= ;

: MDLT-NO-FALSE-POSITIVE ( -- )
   \ `\` line comments are stripped -> mentioning maki/ in prose is NOT a finding
   s" \ this loads maki/array.f at the maki layer" MDL-COUNT 0 T=
   \ `( )` stack-effect comments are stripped too
   s" : F ( maki/x -- n ) dup ;"   MDL-COUNT 0 T=
   \ clean core code never trips
   s" : SQUARE ( n -- n ) dup * ;" MDL-COUNT 0 T= ;

: MDLT-LIVE-LINT ( -- )
   \ the real tree (src/ lib/ test/) must be maki-free -> MAKI-DEP-LINT returns clean
   MAKI-DEP-LINT ;

: MDLT-MAIN ( -- )
   T-RESET
   MDLT-EXT
   MDLT-DETECT
   MDLT-NO-FALSE-POSITIVE
   MDLT-LIVE-LINT
   T-REPORT ;

MDLT-MAIN
