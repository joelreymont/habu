\ bundle-lib-test.f - thin entry for bundle-lib-test-lib.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require tools/cli-run.f
require tools/bundle-lib-core.f

require tools/bundle-lib-test-lib.f

BLTT-MAIN
