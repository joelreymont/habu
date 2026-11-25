---
title: Read code words through LE
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:15:11.038097+01:00"
---

Problem: src/habu/address-carrier.f (W32@, W32!) and src/arch/arm64/disasm.f (U32@) each keep a private copy of src/lib/le.f's LE:U32@ and LE:U32!. Acceptance: they call LE's words, or the load order that rules it out is stated beside the copy; the engine converges. Depends: the x86 stack's landing. Claim: unassigned.
