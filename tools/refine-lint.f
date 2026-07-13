\ refine-lint.f - CLI entrypoint for the TRUSTED refinement-mint confinement lint.
\ Load after tools/refine-lint-core.f.
\ Run: bin/hb --load tools/refine-lint.f

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/trust-lint-core.f
require tools/refine-lint-core.f

: RFL-MAIN ( -- )
   [: REFINE-LINT ;] catch {: code:n :}
   s" refine-lint" code LINT-MAIN ;

RFL-MAIN
