---
title: Interpret literal keywords in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T17:23:41.532885+03:00"
---

Problem: the interpret-mode literal keywords are dispatched before find and are not records (`habu2.f:8352-8362`), so under I4's seam `s"` `c"` `."` `s\"` `c\"` `.\"` `char` `'` die E-UNDEFINED.
Acceptance: a keyword step before `NUMBER` in `OUTER`'s loop: `C-ISDQ`/`C-ICQ`/`C-IDOTQ` and their escaped twins (`habu2.f:4452-4528`, decoder `4330-4365`); `c"` over 255 bytes rc 76 via the compile-die tail; an unterminated quote rc 74; `char` (`4530-4538`) and `'` (`4549-4568`) with no name rc 74; `C-QUALIFY-SEAL-GUARD`; the WIDE/INT fail-closed refusals; an undefined tick is a no-op; the used-publics retry; hook events `TOP-EV-STR/CSTR/CHAR/TICK`.
Files: `src/habu/outer.f`, cases beside `test/outer-interpret.f`.
Verify: spark: the route-equality cases through `--load` (the `test/top-row-hook-test.f:125-147` window rows are the oracle); the gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: `src/habu/outer.f`, the test).
Ownership: krait (Intel lane).
Claim: unassigned.
