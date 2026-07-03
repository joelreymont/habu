---
title: "TFAM 2c: AOT protected-WID persistence + WIDN advance"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.919362+02:00\""
---

PLAN.md item 2 (AOT half). Persist protected-WID registry through seed capture/restore with widened WID field (no u8 truncation, src/habu/aot-capture.f:133-160); restore WIDN above max restored WID before user wordlist/package allocation; reject sealed/generated WIDs in record registration, relocation lookup, bootrun; snap-rebase friend-only or protected-range checked. Fixtures: WIDs >255 round-trip. Gate 17b. Depends: TFAM 2b.

## PROGRESS (2026-07-04)
WID-width slice LANDED (commit "Widen AOT seed WID field past u8"): compact
record 8->12B, wid u32, WIDN advanced past restored wids at boot,
ACAP-WID-SELFTEST regression in the metabuild. REMAINING (blocked on TFAM 2b):
persist the protected-WID registry through seed capture/restore, reject
sealed/generated WIDs in record registration/relocation/bootrun, snap-rebase
friend-gating, and a boot-time integration test with captured wid>255 records
(needs 2b's protected-WID producer).
