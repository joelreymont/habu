---
title: Migrate tools/date.f PARSE-YMD/DATE-N to option (wide radius)
status: closed
priority: 2
issue-type: task
created-at: "2026-07-10T20:29:15.597433+02:00"
---

The Gforth-compat copy tools/date.f:126,135 (census-switchover.md:91) still returns n bool sentinels; lib/date.f's PARSE-YMD/DATE-N are option<n> already (commits b43dd57f-era + 959d3abf). Consumers of the tools copy: trust-lint-core.f, trust-lint.f, stale-status-lint-core.f, check.f, gate-diagnostics*, date-test.f, run-worker-diag* (~8+ files, several test/-owned). Migration needs: tools/date.f DATE-N -> option<n>, PARSE-YMD -> option<n>, require lib/adt/option.f in the bundle (verify Gforth-bootstrap compat: option.f must load under the recovery host or the bundle stays a documented boundary), then rewrite all consumer MATCHes. Do as a dedicated batch; owning gates: date-test, trust-lint self-test, stale-status-lint, gate-diagnostics slices, test/run.f. tfam lane.

## LANDED (single commit — atomic finder pair + consumers)

GFORTH-COMPAT VERDICT: COMPATIBLE — the concern was moot. Evidence: the gforth
stage of tools/bootstrap.sh (SRC_COMMON, :65-92) loads ONLY src/core, src/arch,
src/os, src/habu — never tools/ or lib/; bootstrap/ (the gforth host tree) has
zero references to tools/date.f or lib/adt; every tools/date.f load path in the
repo is native `bin/hb --load` (incl. the bootstrap.md:144 fixpoint-install line,
which runs under the freshly built native binary where require works normally).
So `require lib/adt/option.f` in tools/date.f never reaches gforth.

Migration: tools/date.f DATE-N + PARSE-YMD -> option<n>, bodies mirroring
lib/date.f's landed shapes exactly (guard-and-continue MATCH chain in PARSE-YMD).
DATE-N has zero callers outside PARSE-YMD. PARSE-YMD consumers rewritten (8 sites,
7 files; exhaustive sweep incl. examples/maki — none there):
- tools/trust-lint.f:63 TL-ARGV-CONFIG-TODAY (none -> TL-ARGV-BAD-TODAY, throws),
  :76 source-list-today probe (none -> 2drop TL-FALSE exit)
- tools/trust-lint-core.f:641 TL-CHECK-AUDIT-DATE (none -> TL-BAD-AUDIT-DATE exit)
- tools/stale-status-lint-core.f:332 SS-PARSE-TODAY (none -> SS-BAD-TODAY, throws),
  :357 SS-CHECK-STATUS (none -> SS-BAD-STATUS-DATE exit; some -> drop, validate-only)
- tools/trust-lint-test.f:301 TLT-TODAY>N (none -> E-FS-PATH throw)
- test/gate-diagnostics-lib.f:746 GDX-TRUST-LINT-TODAY (none -> GE-FAIL)
- tools/date-test.f DATE-PARSE= / DATE-PARSE-BAD (direct both-branch tests)
No `PARSE-YMD 0=` sentinel test remains anywhere. tools/ manifest-exempt
(verified: no date rows).

BEHAVIOR IDENTITY PROVEN (PARSE-COUNT method, via a master-overlay tree so the
old copies load their own old date.f): trust-lint (. 2026-07-10) and
stale-status-lint (. 2026-07-10) plus the bad-today variants — all four runs
BYTE-IDENTICAL output and identical exit codes (0/1) migrated vs master.
Owning gates green: date-test, trust-lint-test, stale-status-lint-test.
