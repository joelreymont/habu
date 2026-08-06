\ duplicate-definition-lint-test.f - thin entry for duplicate-definition-lint-test-lib.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require lib/json-write.f
require tools/duplicate-definition-lint-core.f

require tools/duplicate-definition-lint-test-lib.f

DUPLICATE-DEFINITION-LINT-TEST:RUN
