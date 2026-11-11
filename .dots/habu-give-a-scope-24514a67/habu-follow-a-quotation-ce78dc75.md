---
title: "Follow a quotation's record in the arm64 kernel"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T08:16:31.818988+02:00"
---

Problem: a quotation that reads locals is the address of a record in its word's frame with bit 63 set (DECISIONS.md 'quotations read locals'); the arm64 kernel calls a quotation's cell as a code address (src/habu/habu1.f BEXEC :3670, BCATCH-RUN :3688, BC2-INVOKE's finisher :4306). Design (lead, from the T0 census): a kernel piece BRECORD before BCALLABLE at those three calls: bit 63 set, x4 := the record's address, x9 := its first cell; x4 because x0-x5 are unwritten between pop and BLR and T0 never writes them. The finisher call also gains BCALLABLE (it had no unset check or fence). Acceptance: plain quotations run as before (test/finally.f, catch-frame.f, empty-quotation.f, c2-memory-runtime.f on an engine built from the lane); the bit-63 path is proved by the T0 lane's tests. Files: src/habu/habu1.f. Verify: the brief's build, tests and disassembly. Depends: none. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/quot-kernel-arm64.md. Ownership: src/habu/habu1.f in the lane. Claim: agent=jerry workspace=.jj-ws.noindex/quot-kernel.
