\ gate-common.f - entry wrapper for native gate helpers.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

include test/gate-stats.f
include lib/content-key.f
include test/gate-common-lib.f
