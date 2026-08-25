\ gate-stdlib.f - entry wrapper for the native test suite.
\
\ It requires its own dependency prefix so test/run.f has no hidden setup.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f
require lib/content-key.f
require test/gate-stdlib-lib.f
include test/gate-stdlib-cases.f
