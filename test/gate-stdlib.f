\ gate-stdlib.f - entry wrapper for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test/runner.f,
\ test/gate-pool.f. Native bin/hb already carries src/core/sha256.f.

require test/gate-stats.f
include lib/content-key.f
include test/gate-stdlib-lib.f
include test/gate-stdlib-inline-lib.f
include test/gate-stdlib-cases.f
