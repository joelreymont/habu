\ stdin-closure-lint.f - CLI entrypoint for the stdin driver closure drift gate.
\ Run: bin/hb --load tools/stdin-closure-lint.f
\ Enforcing: prints the drift count and THROWS on any finding (gate 17e).
\ Load after tools/stdin-closure-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/bootstrap-src-lib.f
require tools/stdin-closure-lib.f
require tools/stdin-closure-lint-core.f

\ The entry is this package's own public word, the shape tools/maki-dep-lint.f
\ uses: nothing here is global.
package STDIN-CLOSURE-LINT-CLI

public

: MAIN ( -- )
   [: STDIN-CLOSURE-LINT:STRICT ;] catch {: code:n :}
   s" stdin-closure-lint" code LINT-MAIN ;

;package

STDIN-CLOSURE-LINT-CLI:MAIN
