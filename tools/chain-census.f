\ chain-census.f - CLI wrapper for the native-chain dialect census.
\ Load after tools/chain-census-core.f and lib/argv.f.
\
\ Run it from the repository root: every path is handed to `required` and to the
\ file reader unchanged, and the tree's own require paths are relative to the
\ root. A path may be a file or a directory; a directory is walked for its Habu
\ sources. Whichever way they arrive, the whole list is sorted before anything is
\ censused, so the report reads the same on every host.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/sort.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/def.f
require tools/chain-census-core.f
require lib/argv.f

\ THE ENTRY IS CALLED WITH NO PACKAGE OPEN, and that is a requirement rather than
\ a style. The census loads each file it measures, and a file that opens a package
\ inside an already-open one is source the engine refuses outright - it exits on
\ the nested opener, before anything catchable happens. So the run stands after
\ `;package`, where the sibling lint entries call it from inside their own private
\ section: those tools read files, this one LOADS them. Nothing global is defined
\ here; the entry is this package's own public word.
package CHAIN-CENSUS-CLI
public

: RUN ( -- )
   s" tools/chain-census.f path ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   CHAIN-CENSUS:RESET
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ CHAIN-CENSUS:PATH+
      1+
   repeat drop
   CHAIN-CENSUS:RUN
   CHAIN-CENSUS:REPORT ;

;package

CHAIN-CENSUS-CLI:RUN
