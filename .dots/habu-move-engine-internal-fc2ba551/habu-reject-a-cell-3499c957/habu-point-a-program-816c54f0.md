---
title: "Point a program's data-base at no unrelated word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.072264+01:00"
blocks:
  - habu-keep-the-data-a062de26
---

Problem: on the jerry-dbp engine a program's data-base is rejected E-UNDEFINED with help '`data-base` is public in `WPROF`: write `WPROF:data-base`'. WPROF:DATA-BASE (src/arch/wasm/profile.f:46) is the Wasm profile's layout constant, so following the help certifies a program that reads a wrong number. Acceptance: that rejection's help names no word of another package that is not the engine's data-base, under hb --load and tools/check.f -- --verify-only. Depends: habu-keep-the-data-a062de26. Claim: unassigned.
