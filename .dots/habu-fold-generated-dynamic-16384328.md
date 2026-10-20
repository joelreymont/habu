---
title: Fold generated dynamic-buffer accessors
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.256713+03:00"
---

Otto’s /tmp/otto-bb-zzu.md measured first DYNAMIC-BUFFER runtime closure+4,008 code bytes and additional declarations+148 bytes each in zzusage;20blackboard buffers use3,016 bytes. Examine current generated accessor/reserve/release words and lower uses through authoritative storage semantics where that actually reduces whole-program code. Preserve indexing, allocation/growth, stable-slot semantics and failures; no deleting used runtime or blind inlining. Acceptance: existing dynamic-storage E2E success/refusal paths and changed-program artifacts, before-after same-source code/size measure, native/convergence gates and Astra review.

Matched native census on the rebuilt 5b93a9ee engine reduces four generated DB words from160 to84bytes; the stripped probe retains448 fewer code bytes, while page padding leaves the shipped file at49788bytes. The direct accessor's measured read cost is35.85 versus70.11 retired instructions; shared-runtime reachability and the combined required gates remain separate acceptance boundaries.
