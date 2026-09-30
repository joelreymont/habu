---
title: Move packages, using and EXPORT into Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.680527+03:00"
---

Problem: `package`, `public`, `private`, `;package`, `using`, `;using` and `EXPORT` run in the assembly interpreter.
Acceptance: those words in `src/habu/packages.f`, with `E-USING-SHADOW-GLOBAL`, the 16 limit and the checker notifications (the `C-CALL-CHECKER-PACKAGE` family) as today.
Files: `src/habu/packages.f`.
Verify: spark: the gate's package and using suites through the Habu loop under the feature cell; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: src/habu/packages.f).
Ownership: krait (Intel lane).
Claim: unassigned.
