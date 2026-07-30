\ render-parse-lint.f - CLI entrypoint for the renderer-output fence.
\ Run: bin/hb --load tools/render-parse-lint.f
\ Enforcing: prints the ledger and THROWS on any finding. Design section 6.6 says
\ the renderer's text is never parsed by the compiler; this gate makes that
\ unreachable rather than merely written down, by refusing any source under
\ src/compiler/ - other than the renderer stage's own two files - that names a
\ word of IR-RENDER or IR-DIFF, opens either package, or loads either source.
\ Load after tools/render-parse-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/render-parse-lint-core.f

package RENDER-PARSE-LINT-CLI

: RUN ( -- )
   [: RENDER-PARSE-LINT:STRICT ;] catch {: code:n :}
   s" render-parse-lint" code LINT-MAIN ;

RUN

;package
