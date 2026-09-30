---
title: Route stdin, REPL and application entry in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.974480+03:00"
blocks:
  - habu-parse-argv-and-2106dd5c
  - habu-recover-throws-across-18bd36f9
---

Problem: input routing is assembly: `LREPLROUTE`, `SRC-PIPEOK` and `C-SOURCE-STDIN` (`habu2.f:1300-1310,1742-1752,1864-1874`).
Acceptance: `src/habu/main.f` routes as those do: the tty probe, `REPLH-CELL`, the `APP-ENTRY:XT-CELL` conventions, the exit hook (`EXIT-HOOK-CELL`).
Files: `src/habu/main.f`, `src/habu/repl.f` (read hook), cases in I10a's test.
Verify: spark: stdin, tty and application-entry cases; gate.
Depends: habu-parse-argv-and-2106dd5c (I10a), habu-recover-throws-across-18bd36f9 (I9c).
Route: Alder (shared: src/habu/main.f, src/habu/repl.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.

K-lane correction (design 2026-09-30): Acceptance add "on x86 stores the uncaught-throw reporter xt `( n -- )` in `UNCGH-CELL`" (K7's no-handler `throw` calls it); Files add `src/habu/kernel-x64.f` only if the store needs a kernel row.
