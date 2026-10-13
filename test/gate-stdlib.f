\ gate-stdlib.f - entry wrapper for the native test suite.
\
\ It requires its own dependency prefix so test/run.f has no hidden setup.

require src/lib/errors.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/runner.f
require test/gate-pool.f
require src/lib/content-key.f
require test/gate-stdlib-lib.f
include test/gate-stdlib-cases.f
