---
title: Move the string literal store to INTERN
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T16:03:22.008720+02:00"
---

Joel 2026-11-03: NSTR (src/compiler/native/string.f) is the string literal store; "call the package INTERN then?" then "do it". One store moved to src/habu/intern.f, package INTERN, not a T0 copy; tier 1's calls follow. Design /tmp/jerry-big/intern-design.md (lanes: move and rename; dead words out with entry for row; COUNT/BYTES out). Critic round 1 running. Public names to Joel one per question after the critic. Claim: agent=jerry.
