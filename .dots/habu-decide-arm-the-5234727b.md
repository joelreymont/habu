---
title: "Decide: arm the AOT seed for batch boots"
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T17:49:45.580264+02:00"
---

USER DECISION REQUIRED (product surface). The AOT seed - the tree's chosen replacement for retired warm images - is tty-armed: it runs only on interactive REPL entry (habu2.f:7305 gate, SRC-REPL arm at :1433), so baked code serves ZERO of the gate's 323 batch/piped engine boots. Making baked code pay (chain bake, prefix bake, the 124s/gate prefix-recompile bill) requires arming the seed for --load and piped boots too - which changes what names exist in every batch program's dictionary (today a batch program cannot see baked-only words; after, it can). Options: arm everywhere (one dictionary surface, simplest, biggest behavioural change), arm behind a flag (a mode - two dictionaries, the tree dislikes modes), arm everywhere + keep the baked set identical to the source set (no observable difference once bake reaches parity - the honest end state). Blocked on: nothing technically; blocked on the user's call. Files: src/habu/habu2.f (the arm), docs. Depends: none.
