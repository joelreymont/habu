---
title: Package breakpoint debugger
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:50:36.886225+02:00"
---

Current master census: src/habu/debug.f:10-81 adds 23 unowned definitions and 161 definition-name bytes to the AOT-seeded debugger bundle. Eight globally callable TRUSTED helpers at 22-43 expose slot pointers, saved instructions, hit/control cells, raw code-pointer casts, address printing, and patch32 mutation even though users need only BP+, BP*, BPN, BP-, and BP. Put the module in package BREAKPOINT. Keep those five documented commands as the deliberate public/global surface or qualified equivalents chosen by the REPL contract; make W32 access, slots, lookup/free, control packing, pointer casts, instruction patching, installation, and table mutation private. Update stdin.f boot wiring and debugger callers directly without aliases. Preserve one-shot, persistent, skip-N, remove/list, BRK signal resume, instruction restoration, duplicate/capacity errors, AOT/snapshot/fixpoint, and bare-binary debugging. Prove old trusted/raw helpers and qualified private fields reject; all five commands retain byte-exact output and behavior; injected signal/install/remove failures restore code exactly; full table/canaries/reset/reuse; protected code boundaries remain enforced. Measure dictionary-name/JIT/DATA/CODELEN and command latency before/after. Serialize with the typed dictionary-record and debug signal-contract owners where their files overlap.
