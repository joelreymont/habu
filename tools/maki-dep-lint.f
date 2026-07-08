\ maki-dep-lint.f - CLI entrypoint for the one-way habu<-maki dependency lint.
\ Load after tools/maki-dep-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/maki-dep-lint-core.f

: MDL-MAIN ( -- )
   [: MAKI-DEP-LINT ;] catch {: code:n :}
   s" maki-dep-lint" code LINT-MAIN ;

MDL-MAIN
