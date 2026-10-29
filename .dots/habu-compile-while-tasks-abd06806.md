---
title: "Compile while tasks run: per-thread W^X"
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T14:30:00.007048+02:00"
---

Lift task.f 'no compilation while a task is live' ($4F refusal on TASKS-LIVE-CELL). Cause: the region's three bands (records, CF stack, code; layout.f 'the region's three write bands') are flipped RW/RX by process-wide mprotect (habu1.f EMIT-PROT, LPROTREC), so a task executing in a flipped unit faults. Design: map the region (habu2.f EM-MMAP-CODE-REGION) MAP_JIT on macOS arm64 and toggle write per thread with pthread_jit_write_protect_np; on Linux/x86-64 a dual memfd mapping. Compile appends past CP, so publish = icache flush + release store of the wordlist head; one compile lock for dictionary pointers/links; forget stays refused while tasks are live. hb has no hardened runtime (codesign flags 0x0), so MAP_JIT needs no entitlement. Joel 2026-10-29: start a lane now. arm64 first (one-arch-first). Workspace .jj-ws.noindex/carl-wx, spike brief carl-wx/.out/brief-wx-spike.md.
