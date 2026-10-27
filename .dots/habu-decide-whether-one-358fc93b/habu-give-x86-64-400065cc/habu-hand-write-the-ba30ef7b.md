---
title: Hand-write the x86-64 primitive bodies
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T22:35:09.969956+02:00"
closed-at: "2026-10-28T13:34:02.192471+02:00"
close-reason: "Landed: x86 kernel images match master on the peer (251/251 exit statuses), prim-parity ok"
---

Port lane L3 (design jerry-aot/.out/notes/fable-tier0-port-design.md): replace the 61 PRIM-HIR/SCALAR-HIR rows of src/habu/kernel-x64.f (from 5463) with hand-written X64ASM bodies through a few shared helpers modeled on the hand-written rows; delete src/habu/kernel-hir-x64.f and its requires. Proof: x86-64-kernel-pure and test/prim-parity.f on the x86-64 peer, cross-built x86 engine. Claim: jerry, workspace jerry-x64-prims.
