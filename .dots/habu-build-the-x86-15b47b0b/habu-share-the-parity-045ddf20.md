---
title: Share the parity case sets as data
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T14:48:39.759522+03:00"
---

Problem: the integer case sets of `test/prim-parity.f` (590-954) run only through PARITY's own shape words, at load; the x86 kernel test (K5 habu-emit-x86-pure-f70fb84b) cannot run them without copying them, and `ptr-field` has no case set.
Acceptance: new `test/prim-cases.f` holds those case sets verbatim, except the coverage self-checks (613-619) and the two `MARK-PAIRED` lines (923, 930); it adds a `0 CASES ptr-field` set of `PN-P` cases `0 0 0`, `0 1 8`, `8 2 24`; it defines no word, and its header states what the including file provides (`CASES ( n -- )` parsing a name, `;CASES`, the fourteen shape words, `MAX-N`, `MIN-N`, `E-DIV-ZERO`, a `+!` scenario that adds 3). `prim-parity.f` includes it with `s" test/prim-cases.f" included` inside PARITY where the sets stood (not `require`, so a second include is never skipped); the coverage self-checks run before the include over their own one-case `0 CASES +` set, the `MARK-PAIRED` lines after it. `PN-P-PRIM` gains the arm `FIX-BYTES a + b ptr-field byte-view FIX-BYTES -`. Float sets stay in `prim-parity.f` (K13 moves them).
Files: `test/prim-cases.f` (new), `test/prim-parity.f`, the parity bullet of "The primitive table" in `docs/x86-64.md`.
Verify: spark `bin/hb --load test/prim-parity.f` green and reports `ptr-field` covered; before the change the same run lists `ptr-field` under "rows with no case set".
Route: direct (test-only).
Ownership: krait (Intel lane).
Claim: unassigned.
