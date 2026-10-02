---
title: Keep argv paths out of interpreted source
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.324213+03:00"
---

Problem: both loops load an argv file by building source text around its path: the engine's `C-SOURCE-FILE-LOOP` rows (`LAPPPROV`/`LAPPREQ`, src/habu/habu2.f) and main.f `ROW` (src/habu/main.f) write `s" <path>" provided` / `script-required` and interpret it. A path holding `"` ends the string early and the rest runs as code (a file named `x" 1 die` runs `1 die`); a path that legitimately contains a quote cannot be loaded. Found by Astra reviewing I10a (`tsowzlus`).
Acceptance: the path reaches `provided`/`script-required` as a string argument, never as source text, in both loops; any path the OS accepts loads by name, and no byte of it is interpreted. Tests: a file whose name holds `"` and a Forth word loads and runs its own contents only, in both loops (test/main-argv.f), through the real argv path.
Files: src/habu/habu2.f (the argv rows), src/habu/main.f, test/main-argv.f.
Verify: test/main-argv.f; chain and gate (engine rows change).
Depends: none.
Ownership: krait.
Claim: unassigned.
