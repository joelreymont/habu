\ hb-build.f - the native application build command (tools/hb-build-lib.f).
\   bin/hb --load tools/hb-build.f -- [--engine [--whitebox] [--target T]
\     | --repl | --json-errors] [--size-report] [--report-json] [source.f] -o out
\ A stripped program, or under --repl hb with the source's files, linked in this
\ process (tools/hb-build-core.f). The source defines a checked global MAIN ( -- ),
\ any other effect refused, which --repl's hb runs before its REPL; top-level code
\ runs once, at build time.
\ HB_BUILD_TIMEOUT_MS: wall-time deadline of an --engine build's child.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/content-key.f
require src/lib/json.f
require tools/hb-build-report.f
require tools/hb-build-lib.f

HB-BUILD-CLI:HBB-MAIN
