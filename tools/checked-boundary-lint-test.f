\ checked-boundary-lint-test.f - thin entry for checked-boundary-lint-test-lib.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test/outcome.f
require tools/cli-run.f
require tools/lint/text.f
require tools/lint/json-writer.f
require tools/checked-boundary-lint-core.f

require tools/checked-boundary-lint-test-lib.f

CBLT-MAIN
