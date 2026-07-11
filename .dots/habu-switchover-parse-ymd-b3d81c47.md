---
title: "Switchover: PARSE-YMD (lib/date.f) value+flag -> option<n>"
status: closed
priority: 3
issue-type: task
created-at: "2026-07-10T13:13:22.000000+00:00"
---

Wave A (dot habu-switchover-wave-a-54edcee6, epic habu-epic-adopt-adts-64833911).
`PARSE-YMD` (lib/date.f) returns `( ptr u8 n -- n bool )` (Unix epoch day + success
flag) → migrate to `( ptr u8 n -- option<n> )`. Deferred from the DATE-N slice
because PARSE-YMD has a WIDE external caller radius (~5 sites across 3 tool files),
so it needs its own dedicated slice, not the one-file DATE-N batch:

- `tools/trust-lint-core.f` (TL-M-AUDIT$ ... PARSE-YMD 0=)
- `tools/trust-lint.f` (2 sites: ARGV date validation)
- `tools/stale-status-lint-core.f` (2 sites: SS-DATE$ / status-date validation)
- tests: `tools/date-test.f` (DATE-PARSE= / DATE-PARSE-BAD), `tools/stdlib-date-test.f`

Each caller does a `PARSE-YMD 0=` sentinel test → rewrite to `MATCH option none OF
... ENDOF some OF ... ENDOF ;MATCH`. Update the PARSE-YMD manifest row
(lib/std.manifest, currently `(ptr u8 n -- n bool)`) — confirms public-sig renders
the option return. Reuse the shared `lib/adt/option.f` (already required by date.f
as of the DATE-N slice); no new public family. DATE-N (the inner field parser) is
already option<n>, so PARSE-YMD's internals already MATCH it — this dot only
migrates PARSE-YMD's OWN return + its external callers. NO new trust rows.
