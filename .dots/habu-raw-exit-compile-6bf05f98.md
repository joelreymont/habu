---
title: Raw-exit compile errors to catchable throws
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T04:47:03.792848+02:00"
---

Runtime-compiler errors that terminate via NR-EXIT-GROUP instead of throw are not catchable by the evaluate result object (habu-evaluator-result-obj-2cf9b484): duplicate-definition exit 0x4E (src/habu/habu2.f ~1451), colon/dict overflow 0x4C/0x4D (habu2.f ~2584-2588), package misuse 0x4A/0x4B (C-PACKAGE-FAIL habu2.f:2638), quotation-local ref exit 75 (habu2.f:2322), C-DIE-DOES checker-reject exit 70 (habu2.f:1174). Convert the RECOVERABLE ones to throw named codes so evaluate can roll them back in-process; keep genuinely-fatal ones (DP-CHECK data overflow, mmap/snapshot corruption) as die. Touches FIND/HIDX + colon regions. Unblocks the general candidate-side batched probe RUNNER (probe table -> rc/stdout/stderr per probe) for raw-exit source-misuse negatives in habu-batch-candidate-src-57288926.
