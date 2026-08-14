---
title: Arm the AOT seed for every boot mode
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-11T17:49:45.580264+02:00\""
---

USER DECISION REQUIRED (product surface). The AOT seed - the tree's chosen replacement for retired warm images - is tty-armed: it runs only on interactive REPL entry (habu2.f:7305 gate, SRC-REPL arm at :1433), so baked code serves ZERO of the gate's 323 batch/piped engine boots. Making baked code pay (chain bake, prefix bake, the 124s/gate prefix-recompile bill) requires arming the seed for --load and piped boots too - which changes what names exist in every batch program's dictionary (today a batch program cannot see baked-only words; after, it can). RULED BY THE USER 2026-08-11: ARM EVERYWHERE. One dictionary surface for every boot mode - batch programs gain the baked-only words, the baked set becomes part of the engine's contract. The rejected options for the record: a flag (a mode - two dictionaries), and parity-gated arming (delays the gate payoff until bake parity is proven). Implementation: remove the SRC-REPL-only arm (habu2.f:1433) so AOT-SEED-ARM-CELL is set on every source mode, with the seed's existing done-cell guard unchanged; the tty-armed contract sentences added at AOT-BOOTRUN-CAP and BOOTRUN+ must be updated in the same commit (they become 'the seed runs on every boot'); the pty-only fixture vehicle note on the aot-wid suite relaxes (batch fixtures become possible - simpler tests). Acceptance: a baked-only word resolves under --load and piped stdin AND on a pty; the full gate green; the batch/interactive dictionary surfaces identical (probe both). Files: src/habu/habu2.f (the arm + prose), src/habu/aot-capture.f (prose), docs. Depends: none technically; sequenced with the bake work (widen-the-aot 089f5faf) since arming pays when there is something baked to serve.

Claim: agent=arm-seed workspace=.jj-ws/habu-arm-seed
