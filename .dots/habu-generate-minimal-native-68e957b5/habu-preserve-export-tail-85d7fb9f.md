---
title: Preserve EXPORT tail across checker call
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T19:53:04.953209+02:00"
blocks:
  - habu-emit-one-shared-dae6cd61
---

Measured at master 3909bbac. C-EXPORT-TAIL! emits a 21-instruction, 84-byte qualified-name tail scan. C-EXPORT expands it at src/habu/habu2.f around 4543, restores the original name for C-CALL-CHECKER-EXPORT, then expands and executes the same scan again around 4550; the source explicitly calls the second pass a pure rescan. Root cause: the first derived tail address/length are discarded across the checker call even though C-EXPORT already owns a stack frame. Fix: compute one immutable tail descriptor, preserve it across the checker/W^X transition in the frame or a typed descriptor, restore TKA/TKL from that descriptor for publication, and delete the second scan. Keep the original spelling separately for checker diagnostics and qualification checks. Acceptance: disassembly proves one 84-byte scan, one checker call, and no rescan; exact C-EXPORT and CODELEN shrink is recorded after descriptor save/restore cost; bare, leading-colon, trailing-colon, qualified, malformed double-colon, public/private package, duplicate, protected-WID, long-name, checker-reject, evaluate rollback, AOT, snapshot, bootstrap mirror, clobber lint, fixpoint x2, both targets, full gates, and exact ratchets pass. Serialize after habu-emit-one-shared-dae6cd61 because both edit C-EXPORT publication. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, EXPORT/package tests, engine-size attribution, and size gates.
