\ chain-census-test.f - thin entry for chain-census-test-lib.f.
\
\ MAIN is called from OUTSIDE the package, and that is load-bearing rather than
\ tidy: the census LOADS the fixtures it measures, and a file that opens a package
\ inside an already-open one is source the engine refuses outright. The suite must
\ therefore run with no package open, exactly as tools/chain-census.f does.
\
\ It also runs with the repository root as the working directory, because the
\ census's own dependencies are required by root-relative path.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/sort.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/def.f
require tools/chain-census-core.f

require tools/chain-census-test-lib.f

CHAIN-CENSUS-TEST:MAIN
