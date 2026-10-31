---
title: Correct the interpreter claim in architecture.md
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:37:06.914427+02:00"
---

Problem: docs/architecture.md 'Interpreter' says the interpret loop, definers, packages and source loading are written once in typed Habu and run on every platform, but src/habu/native-runtime.f LOAD-HABU-LOOP loads src/habu/runtime-x64.f (which alone requires main.f, interpret.f, definers.f) only for linux-x86-64: arm64 keeps the kernel's bodies and its map holds no word from main.f, interpret.f, definers.f, outer.f or packages.f (lane tp2, /tmp/jerry-big/bvr-answer.md). Consequence: an edit to those files is proven on arm64 only by whitebox rows that source-load them (main-argv) or the x86-64 cross-build. Acceptance: the doc states what is true now or drops the claim (docs never sync code: the fact sits in native-runtime.f's comment); if arm64 is meant to run the typed loop, a dot for that move. Files: docs/architecture.md. Verify: read. Depends: none. Ownership: docs/architecture.md. Claim: unassigned.
