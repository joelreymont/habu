\ dot-dep-lint.f - CLI entrypoint for dot dependency lint.
\ Load after tools/dot-dep-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/dot-dep-lint-core.f

DOT-DEP-LINT
