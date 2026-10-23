\ hb-build.f - the native application build command.
\
\   bin/hb --load tools/hb-build.f -- [--engine [--whitebox] [--target T]
\     | --repl | --json-errors] [--size-report] [--report-json] [source.f] -o out
\
\ Without --repl or --engine it writes a stripped AOT image of source.f;
\ --repl writes a snapshot application that keeps the native compiler, the
\ checker and the REPL; --engine builds an engine (--whitebox a whitebox one,
\ --target a named build profile). The source defines a checked global
\ MAIN ( -- ), called once in each new process; any other effect is refused.
\ Top-level code runs once, at build time, and is not replayed at startup.
\ --json-errors writes checker refusals as JSON records (tools/diag-code.f).
\ The build prints its output path, or that the image is up to date;
\ --size-report adds the size table and --report-json writes one JSON report
\ object instead, with the same numbers in its `size` field
\ (tools/hb-build-size.f). That object is schema hb-build-report, version 1
\ (tools/hb-build-report.f); package HB-BUILD gives checked in-process clients
\ the same captured state, and its accessors throw E-BUILD-STATUS from the
\ build's RESET until the build completes, so a failed build never reports an
\ earlier success. --preseed-entry, --preseed-seed and --preseed-mode
\ are test entries (tools/hb-build-lib.f). HB_BUILD_TIMEOUT_MS sets the child
\ builder's elapsed-time deadline, 1 to 2147483647 ms, default 600000; it is
\ wall time, not a CPU budget, and applies to builders started after it is set.
\
\ It loads at the caller's tier, outside any executable-build scope: no image
\ captures this process's code. Each maker is a child engine
\ (tools/hb-build-lib.f HBB-RUN-MAKER-CMD and HBB-RUN-APP-CMD) whose own entry
\ holds the build session over the application it loads, and an object hit
\ links the cached maker output.
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-root.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/content-key.f
require src/lib/build-cache.f
require src/lib/json-write.f
require src/lib/object.f
require src/lib/object-cache.f
require src/lib/object-index.f
require src/lib/object-resolve.f
require tools/object-image.f
require tools/hb-build-report.f
require tools/hb-build-lib.f

HB-BUILD-CLI:HBB-MAIN
