---
title: Throw the package-context refusal to its catch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T12:34:32.164775+03:00"
closed-at: "2026-10-01T12:48:07+03:00"
close-reason: "No exit layer: the refusal is caught as 70 (diagnostic on fd 2) and exits 70 unhandled; test/engine-error-package.f proves both. The dot's probe returns 70 from its catch; what exits is a later top-level word event that reaches the tracker's effect query (TR-CERT-DOUT-EMPTY?) under the bare wordlist with a precise row top, e.g. `s\" AFTER\" type`, refused before the word runs with no catch around it."
---

Problem: the package-context refusal exits the process instead of throwing to an enclosing `catch`. The comment above `CHECKER-PKG-CONTEXT-REJECT` (`src/core/checker.f`) says an enclosing catch (a nested `evaluate`, the check tool, a harness) receives a catchable reject and only an unhandled one exits 70. Measured on master 5c32: inside `package UQV`, after `0 set-current`, `s" 2 drop"` through `INCLUDE-EVALUATE` under `catch` prints `hb: no authenticated package context for this definition` and the process exits 70; the catch never returns. The PS lane found it (`habu-restore-pkg-state-d09394b9`, probe `rprobe/uce.f` in its spark cache dir); the base engine behaves the same.
Acceptance: the refusal reaches the innermost `catch` as throw code 70 with the diagnostic on fd 2, and an unhandled one still exits 70. Find the layer that turns the throw into an exit and fix it there. Tests: the uce.f shape (caught: `catch` returns 70 and the next line runs) and an unhandled control (rc 70), in the suite that covers package context.
Files: the layer found; `src/core/checker.f` only if the comment or throw is wrong; the package-context suite.
Verify: that suite; chain if a baked file changes; full gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
