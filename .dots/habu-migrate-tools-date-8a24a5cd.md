---
title: Migrate tools/date.f PARSE-YMD/DATE-N to option (wide radius)
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T20:29:15.597433+02:00"
---

The Gforth-compat copy tools/date.f:126,135 (census-switchover.md:91) still returns n bool sentinels; lib/date.f's PARSE-YMD/DATE-N are option<n> already (commits b43dd57f-era + 959d3abf). Consumers of the tools copy: trust-lint-core.f, trust-lint.f, stale-status-lint-core.f, check.f, gate-diagnostics*, date-test.f, run-worker-diag* (~8+ files, several test/-owned). Migration needs: tools/date.f DATE-N -> option<n>, PARSE-YMD -> option<n>, require lib/adt/option.f in the bundle (verify Gforth-bootstrap compat: option.f must load under the recovery host or the bundle stays a documented boundary), then rewrite all consumer MATCHes. Do as a dedicated batch; owning gates: date-test, trust-lint self-test, stale-status-lint, gate-diagnostics slices, test/run.f. tfam lane.
