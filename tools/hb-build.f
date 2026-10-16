\ hb-build.f - the native application build command.
\ It loads at the caller's tier, outside any executable-build scope: no image
\ captures this process's code. Each maker is a child engine
\ (tools/hb-build-lib.f HBB-RUN-MAKER-CMD and HBB-RUN-APP-CMD) whose own entry
\ holds native compilation over the application it loads, and an object hit
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
require src/lib/object-link.f
require tools/object-image.f
require tools/hb-build-report.f
require tools/hb-build-lib.f

HB-BUILD-CLI:HBB-MAIN
