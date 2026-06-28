\ gate-stdlib.f - entry wrapper for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test-runner.f,
\ test/gate-pool.f, and src/core/sha256.f.

include test/gate-stats.f
include test/gate-stdlib-lib.f
include test/gate-stdlib-cases.f
