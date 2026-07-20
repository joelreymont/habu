---
title: Fix softmax-gradcheck ptxas arch probing
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T22:33:46.391950+02:00"
---

Loose end from the RMSNorm/RoPE kernel landing (c49a7331): tools/ptx/softmax-gradcheck.f throws E-PTXTC-ARCH (-3423) when run standalone on the GB10 because it never sets the ptxas arch; it only SKIPs cleanly inside the gate context. The new device tests established the correct idiom: probe the arch via ATGT:LABEL$ PTXTC:TC-ARCH! (the maki/gpu.f pattern) before emit/assemble - see tools/ptx/rmsnorm-device-test.f and rope-device-test.f headers. Apply the same probing to softmax-gradcheck.f so it runs standalone on any device the box actually has, with a red-first proof (standalone run fails on the unfixed base with E-PTXTC-ARCH, passes after). Audit the other tools/ptx device tests for the same class of gap while there - any that hardcode or omit the arch get the same fix, each with its own standalone-run evidence. Files: tools/ptx/softmax-gradcheck.f (+ any sibling device tests found lacking), no src/.
