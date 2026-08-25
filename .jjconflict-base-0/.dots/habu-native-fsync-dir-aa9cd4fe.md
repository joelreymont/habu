---
title: Native fsync/dir-sync durability primitive
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T23:17:47.014061+02:00"
---

Missing capability found by the atomic-txn landing (habu-v2-atomic-txn-a3c26066, commit c1cb38c8): checked Habu exposes atomic rename (RENAME-FILE/ATOMIC-WRITE-FILE) but NO fsync/fdatasync/directory-sync primitive, so the commit store's recover-old-or-complete-new guarantee holds for PROCESS crashes (page cache survives) but not power-loss/kernel crashes. Work: native fsync + fdatasync + dir-sync primitives (src/habu engine syscalls, arm64 darwin F_FULLFSYNC vs linux fsync distinction documented), stage0 mirror rows if the primitives are baked, checker effect rows, a typed lib/fs surface (SYNC-FILE / SYNC-DIR with errno results per the process-family precedent), and the commit store upgraded: fsync the rev file before ADVANCE-HEAD, dir-sync after each rename, with the crash-test extended to assert the sync calls occur in order (build-the-tool: a sync-call trace knob in the test child, never in production - prove production byte-identical). Engine work: full battery. Acceptance: commit store documents and enforces power-loss durability; primitives fail closed on errno; the durability upgrade proven ordered. Files: src/habu/*, stage0 mirror if baked, lib/fs.f, maki/db/commit-store.f + tests. Ownership: engine durability.
