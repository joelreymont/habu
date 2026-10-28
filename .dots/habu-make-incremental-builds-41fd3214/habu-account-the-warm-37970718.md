---
title: "Account the warm engine's earlier-write-before-load refusals"
status: active
priority: 1
issue-type: task
created-at: "2026-10-24T03:50:28.225095+03:00"
---

Warm hb-build --engine after L2a still shows 31 earlier-write-before-load refusals. Roots lane L6 on the all-reasons census (carl-diag/.out/warm.log, 748a856b): 8 files remain (task, owner-runtime, c2-memory, c2-owner, native-runtime, ffi-abi, float32, codegen); most sites are checker scratch that SCRATCH-RETIRE drops once lanes 3b/3c and L1-L3 span it. Lane: LOAD-ACTION drops owner-span sites as EARLIER-SLOTS does, provided each owner restores its state per segment (owners restored only at the file end stay refused); include.f SOURCE-ROOT cells classed scratch (retired at SCRATCH-RETIRE) or owner. Brief .jj-ws.noindex/carl-l6/.out/brief-l6.md, workspace carl-l6; worker launches after the agent limit resets. Claim: carl. First worker (analysis only, carl-l6/.out/l6-handoff.md): the 7 lib files hold only checker scratch before their loads (they unlock with lanes 3c/L1-L3, not here). Lane cut to the owner-span drop (brief carl-l6/.out/brief-l6b.md). native-runtime.f needs its own lane for all its roots: REQUIRE-REG sites (this lane), REQUIRE-BOOT-V (written before a load, missing from REQUIRE-REG:CELL-SPANS include.f:1024), SOURCE-ROOT scratch (WORK-BUF, ZBUF, OWNER-SAVE, REQUEST-BUF/U: retire with the checker scratch) and result buffers (JOIN, NORMAL, CANON, CANDIDATE, OWNER: a top-level `SOURCE-ROOT:JOIN included` holds them on the stack across the action, so they cannot be retired at SEGMENT-CLOSE; needs a design), plus load-boundary, origin-sink and retire-hook-new.
