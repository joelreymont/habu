\ gate-stdlib.f - entry wrapper for the default gate lint/stdlib phase.
\
\ Standalone entry: it requires its own dependency prefix so
\ `bin/hb --load test/gate-stdlib.f -- <slice>` loads without a caller-supplied
\ prelude. The resident test/run.f path already has these loaded, so the
\ requires are idempotent there. Native bin/hb already carries src/core/sha256.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f
require test/gate-stats.f
include lib/content-key.f
include test/gate-stdlib-lib.f
include test/gate-stdlib-inline-lib.f
include test/gate-stdlib-cases.f
