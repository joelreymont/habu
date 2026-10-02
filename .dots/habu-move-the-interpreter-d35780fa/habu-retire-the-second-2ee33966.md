---
title: Retire the second evaluate entries
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:36:44.695986+03:00"
blocks:
  - habu-move-evaluate-and-9119f746
---

Problem: once I9a lands, the Habu `evaluate` is the evaluate, but two other loop entries remain. `SOURCE-ROOT:INCLUDE-INTERPRET` is the include path in `src/core/include.f` and `src/habu/main.f`, also used by `test/outer-loop-on.f` and `test/outer-interpret.f`. `OUTER:INTERPRET` in `src/habu/interpret.f` is a second entry that the harnesses call directly.
Acceptance: include and main go through the Habu `evaluate` path, and `SOURCE-ROOT:INCLUDE-INTERPRET` is deleted along with its callers' references. `OUTER:INTERPRET` stays only if a production caller needs to interpret the current source without a new evaluate frame, and its comment names that caller. Otherwise the harnesses call `evaluate` and it is deleted. outer-interpret agrees in both loops as before.
Files: `src/core/include.f`, `src/habu/main.f`, `src/habu/interpret.f`, `test/outer-loop-on.f`, `test/outer-interpret.f`.
Verify: outer-interpret; spark chain gen2 == gen3 (baked files); full gate.
Depends: habu-move-evaluate-and-9119f746 (I9a).
Ownership: krait (Intel lane).
Claim: unassigned.
