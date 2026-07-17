\ namespace-lint.f - CLI entrypoint for the governed namespace lint.
\ Run: bin/hb --load tools/namespace-lint.f
\ Enforcing: prints the governed global-definition ledger and THROWS on any
\ finding. The
\ eval/gpu GLOBAL clusters have landed in per-subsystem packages, so the ledger is
\ clean; any new governed global definition outside a package fails the gate.
\ Load after tools/namespace-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/namespace-lint-core.f

: NL-MAIN ( -- )
   [: NAMESPACE-LINT-STRICT ;] catch {: code:n :}
   s" namespace-lint" code LINT-MAIN ;

NL-MAIN
