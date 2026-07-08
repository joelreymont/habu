\ namespace-lint.f - CLI entrypoint for the maki namespace ledger lint.
\ Run: bin/hb --load tools/namespace-lint.f
\ Enforcing: prints the global-maki-def ledger and THROWS on any finding. The
\ eval/gpu GLOBAL clusters have landed in per-subsystem packages, so the ledger is
\ clean; any new global maki def outside a package now fails the gate.
\ Load after tools/namespace-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/namespace-lint-core.f

NAMESPACE-LINT-STRICT
