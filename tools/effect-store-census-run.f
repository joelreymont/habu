\ effect-store-census-run.f - CLI for the effect-store census.
\
\ Marks the store, LOADS the paths it is given, and reports what those loads put
\ in the store. The census loads its subjects, so it runs from the repository
\ root with the tree's own relative require paths, and it stands outside any
\ package because a file that opens a package inside an already-open one is
\ source the engine refuses outright.
\
\ It runs on the unsealed whitebox engine, never the sealed bin/hb, which strips
\ the checker names the census reads (tools/effect-store-census.f says why). Put
\ a copy at bin/hb-whitebox - PROVIDE builds it into the build cache first unless
\ the cache already holds the one keyed to this tree - then census a load:
\   echo 'require test/whitebox-engine.f s" bin/hb-whitebox" WHITEBOX-ENGINE:PROVIDE' | bin/hb
\   bin/hb-whitebox --load tools/effect-store-census-run.f -- lib/json-read.f
\
\ Name files the engine does not already carry. The whitebox engine is the
\ product image with its seal stood down, so every file baked into it -
\ lib/string.f, the compiler chain - is already provided, its require is a
\ registry no-op and the window comes out empty: the trap
\ tools/aot-chain-capture.f documents for its own fixtures.

require lib/errors.f
require lib/string.f
require lib/memory.f
require tools/effect-store-census.f
require lib/argv.f

package EFF-CENSUS-CLI
private

variable MARK-V

public

: RUN ( -- )
   s" tools/effect-store-census-run.f path ..." ARGV:USAGE!
   ARGV:PARSE
   0 -1 ARGV:EXPECT-POS
   EFF-CENSUS:MARK MARK-V !
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ required
      1+
   repeat drop
   MARK-V @ EFF-CENSUS:RUN
   EFF-CENSUS:REPORT ;

;package

EFF-CENSUS-CLI:RUN
