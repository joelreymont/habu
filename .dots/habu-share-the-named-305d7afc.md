---
title: Share the named-row store between the two codegen harnesses
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T09:55:43.245282+02:00"
---

tools/codegen-compare-core.f and tools/codegen-workload-time.f still hold two copies of one named-row store, which is what is left after the timing discipline was factored into tools/codegen-time.f (dot habu-share-the-timing-2eda3703). Both define, identically or near-identically: `SLOT ( ptr a n -- ptr a )`, `ROW-OK`, a `ROW-MAX NAME-MAX * BUFFER: NAME-BYTES` plus `NAME-LENS` string table with `NAME-AT`/`NAME!`/`NAME$`, `variable ROW-N` with `ROWS` and `RESET`, a name-length cap check, and the same `0 begin dup ROW-N @ < while dup NAME$ a u STR= if exit then 1+ repeat drop -1` search (spelled ROW-OF in one and FIND-ROW in the other, the second filtered by path).

Roughly 60 lines are stated twice. What genuinely differs is only parameters and error codes: ROW-MAX 32 vs 48, NAME-MAX 64 vs 32, E-CODEGEN-COMPARE-CAP/ROW vs E-WLTIME-CAP/ROW. A shared store would take the capacities as its own constants (or as one pair large enough for both, decided by measurement, not by taste) and the cap/row code the way tools/codegen-time.f:SPREAD-OF already takes the dead-clock code - from the caller - so each harness keeps reporting its failures under its own names.

Why it is worth doing and why it was not done with the timing dot: the timing dot was scoped to the measurement discipline only and its constraints kept each harness's row plumbing where it was, so it came out net POSITIVE in lines (a new checked package costs about 30 lines of header and boilerplate before its first word). This one is where the net-negative actually is. Same constraints apply: both entry points' printed output must stay byte-identical except the clock-derived digits, and neither harness's error codes may move.

Gates: tools/codegen-compare.f (0 findings), tools/codegen-workload.f rc=0, tools/codegen-compare-test.f, tools/codegen-workload-test.f, tools/codegen-time-test.f, maki/test.f, the two diff linters, error-code-lint, dot lint.
