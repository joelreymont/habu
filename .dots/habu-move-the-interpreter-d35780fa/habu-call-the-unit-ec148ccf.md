---
title: Call the unit hook from the Habu loop
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T14:52:44.697860+03:00"
blocks:
  - habu-move-pkgs-using-22f18b81
  - habu-compile-definer-bodies-1292d049
---

Problem: the engine loop calls the unit hook (UNIT-COMPILE-CELL, armed by `unit-compile-run` for `tools/native-unit-build-core.f`) at five sites and refuses two unit misuses; the Habu loop does none of it, so once I9a makes `evaluate` the Habu loop a unit would load unguarded.
Acceptance: event 0 per interpret token with its number class (`habu2.f:7767-7780`); event 2 once `package` has read its name, a nonzero answer skipping the rest of the input (`8032-8035`); event 3 before a compile-time immediate runs (`7661`); event 4 before a word runs (`8460`); event 5 at the end of the unit's source with its pending-definition refusal (`10221-10230`); the refusal of a `does>` body token and of a qualified definition name (`7781-7784`, `3646-3657`), both rc 70; the token cells restored around each call (`7636-7655`).
Files: `src/habu/{interpret,packages,definers}.f`, `test/outer-interpret.f`.
Verify: `test/native-unit-compile-e2e.f` with its unit loaded through the Habu loop under the switch; a route-equality case whose hook logs (token, class, event) on both routes; gate.
Ownership: krait (Intel lane).
Claim: unassigned.
