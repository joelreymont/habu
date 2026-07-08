\ error-code-lint.f - CLI entrypoint for the global E- throw-code uniqueness lint.
\ Run: bin/hb --load tools/error-code-lint.f
\ Enforcing: prints the collision ledger and THROWS on any finding. A negative
\ code claimed by two different E- names anywhere under src/ lib/ tools/ test/
\ maki/ fails the gate.
\ Load after tools/error-code-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/error-code-lint-core.f

: ECL-MAIN ( -- )
   [: ERROR-CODE-LINT-STRICT ;] catch {: code:n :}
   s" error-code-lint" code LINT-MAIN ;

ECL-MAIN
