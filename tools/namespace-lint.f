\ namespace-lint.f - CLI entrypoint for the maki namespace ledger lint.
\ Run: bin/hb --load tools/namespace-lint.f
\ Report-only: prints the global-maki-def ledger; does not fail the gate yet
\ (the eval/gpu GLOBAL clusters are still awaiting per-subsystem packages).
\ Load after tools/namespace-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/namespace-lint-core.f

NAMESPACE-LINT
