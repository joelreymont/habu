---
title: Avoid unnecessary native return frames
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.611053+03:00"
---

Otto measured8,852bytes of x30 prologue/epilogue code in Dots; estimated1–2KB savings unverified. Determine which native routines can omit return-address save/restore from actual call/control-flow/frame facts. Preserve calls, exceptions, recursion, wide locals and unwinding. Acceptance: same-source executed artifacts and measured code reduction with meaningful affected E2Es before implementation, native suite, convergence and Astra review.

Existing leaf-quotation MIX emits52 bytes on21b4/573305bb versus the earlier recorded60 bytes. Existing native-chain tier1 and inline-primitive suites pass. The retained whole-Dots pair has unchanged return-link transfer counts, so the small affected-case improvement does not establish the original whole-application1–2KB estimate. One attempted native-chain application capture refused out-of-span ROW-CK/MACH-N after its tests passed; no runnable application resulted from that probe.
