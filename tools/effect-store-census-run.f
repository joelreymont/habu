\ effect-store-census-run.f - CLI for the effect-store census.
\
\ Marks the store, LOADS the paths it is given, and reports what those loads put
\ in the store. The census loads its subjects, so it runs from the repository
\ root with the tree's own relative require paths, and it stands outside any
\ package for the same reason tools/chain-census.f does: a file that opens a
\ package inside an already-open one is source the engine refuses outright.
\
\ The chain, which is the window dot habu-the-effect-store-45bdc561 measures:
\   bin/hb-host --load tools/effect-store-census-run.f -- src/compiler/native/migrate.f
\
\ It must be a HOST engine. The product already carries the compiler chain, so
\ loading it there is a no-op and the window comes out empty - the same trap
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
