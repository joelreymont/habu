\ gate-stdlib.f - entry wrapper for the default gate lint/stdlib phase.
\
\ Standalone entry: it requires its own dependency prefix so
\ `bin/hb --load test/gate-stdlib.f -- <slice>` loads without a caller-supplied
\ prelude. The resident test/run.f path already has these loaded, so the
\ requires are idempotent there. Native bin/hb already carries src/core/sha256.f.

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
require test/gate-stats.f
require lib/content-key.f
\ The two libraries are REQUIRED, not included: tools/lint/schedule-lint.f also
\ requires gate-stdlib-lib.f to ask the slice predicate about a label, and a file
\ that arrived here through `include` is not registered, so that require would
\ load it a second time and redefine the package.
require test/gate-stdlib-lib.f
require test/gate-stdlib-inline-lib.f
include test/gate-stdlib-cases.f
