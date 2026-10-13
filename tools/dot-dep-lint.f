\ dot-dep-lint.f - CLI entrypoint for dot dependency lint.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/dot-dep-lint-core.f

: DDL-MAIN ( -- )
   [: DOT-DEP-LINT ;] catch {: code:n :}
   s" dot-dep-lint" code LINT-MAIN ;

DDL-MAIN
