\ checked-boundary-lint-test.f - thin entry for checked-boundary-lint-test-lib.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require tools/cli-run.f
require tools/lint/text.f
require tools/lint/json-writer.f
require tools/hook-sites.f
require tools/checked-boundary-lint-core.f

require tools/checked-boundary-lint-test-lib.f

CBLT:MAIN
