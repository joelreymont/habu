\ nanogpt-inventory-lint.f - CLI entrypoint for the nanoGPT inventory lint.
\ Load after tools/nanogpt-inventory-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/nanogpt-inventory-lint-core.f

: NGI-MAIN ( -- )
   [: NANOGPT-INVENTORY-LINT ;] catch {: code:n :}
   s" nanogpt-inventory-lint" code LINT-MAIN ;

NGI-MAIN
