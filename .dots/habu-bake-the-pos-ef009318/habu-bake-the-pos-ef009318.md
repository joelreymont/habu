---
title: Keep build positions out of a part as plain numbers
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:56:43.985608+01:00"
---

Problem: four engine primitives type a build position as `n` (src/habu/prims.f `cp@`, `ndict@`, `wordlist`, `get-current`), so the checker lets one rest in a raw cell and the part's object copies a number that names a place in the build. With part A grown, a warm build links part B's `ndict@ constant N`, `cp@ constant C` or `get-current constant W` with the old number where a cold build gives a new one (reproducer /tmp/fable-bake-p). `here constant H` is rejected (E-MISMATCH). Design: /tmp/jerry-bake-plan.md (private rows for the four readers; `ndict@` and `cp@` for programs mark the part never-linking; `get-current` and `wordlist` answer the scratch-only type `WL:wid`; `search-wl` takes it; src/habu/prim-ref.f stops baking a wordlist).

Acceptance: test/file-objects.f builds that B after A grows, cold, warm and cold again, with equal outputs and program bytes; a third build with nothing changed compiles B again and gives the same program; `get-current constant W` is rejected the same cold and warm; a part that only probes at top level links warm.

Depends: none. Claim: unassigned.
