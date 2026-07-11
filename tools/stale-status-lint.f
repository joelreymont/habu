\ stale-status-lint.f - CLI entrypoint for stale status/count lint.
\ Load after tools/stale-status-lint-core.f.

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/argv.f
require tools/stale-status-lint-core.f

SS-MAIN
