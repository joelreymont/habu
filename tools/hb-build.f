\ hb-build.f - the native application build command.
\ It loads at the caller's tier, outside any executable-build scope: no image
\ captures this process's code. Each maker is a child engine
\ (tools/hb-build-lib.f HBB-RUN-MAKER-CMD and HBB-RUN-APP-CMD) whose own entry
\ holds native compilation over the application it loads, and an object hit
\ links the cached maker output.
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/build-cache.f
require lib/json-write.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f
require lib/object-resolve.f
require lib/object-link.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/object-image.f
require tools/hb-build-report.f
require tools/hb-build-lib.f

HB-BUILD-CLI:HBB-MAIN
