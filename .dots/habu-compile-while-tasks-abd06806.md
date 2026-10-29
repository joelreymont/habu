---
title: "Compile while tasks run: per-thread W^X"
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T14:30:00.007048+02:00"
---

Lift task.f 'no compilation while a task is live' ($4F refusal on TASKS-LIVE-CELL). Cause: the region's three bands (records, CF stack, code; layout.f 'the region's three write bands') are flipped RW/RX by process-wide mprotect (habu1.f EMIT-PROT, LPROTREC), so a task executing in a flipped unit faults. Design: map the region (habu2.f EM-MMAP-CODE-REGION) MAP_JIT on macOS arm64 and toggle write per thread with pthread_jit_write_protect_np; on Linux/x86-64 a dual memfd mapping. Compile appends past CP, so publish = icache flush + release store of the wordlist head; one compile lock for dictionary pointers/links; forget stays refused while tasks are live. hb has no hardened runtime (codesign flags 0x0), so MAP_JIT needs no entitlement. Joel 2026-10-29: start a lane now. arm64 first (one-arch-first). Workspace .jj-ws.noindex/carl-wx, spike brief carl-wx/.out/brief-wx-spike.md.

Spike facts (carl-wx/.out/wx-facts.md, macOS arm64): 147 region-writing call lines in 90 definitions, all running from engine __text, none from inside the region, so MAP_JIT per-thread toggle (A) is usable. A: MAP_JIT maps under hb's identity with no entitlement; two-thread proof 1000 rounds clean; a write-enabled thread SIGBUSes on region code; toggle 175-188 ns per on/off pair versus 7.4-9.8 us for today's mprotect flip of one 64K unit. B (mach_vm_remap alias): also clean; the hinted RX view keeps the BL-range assertion; 11 sites compute a store address from a code address and would need the alias delta. Design: Fable Plan running, lane list for arm64 macOS to follow.
