---
title: "Build the first engine with its source target layout"
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T14:51:13.073131+03:00"
blocks:
  - habu-preserve-complete-addr-258c0288
  - habu-walk-the-dynamic-e03edf85
  - habu-track-retained-jit-1dc23a17
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own native-build.f phase/output API, aot-capture.f host-to-target capture interface, native-runtime.f SEAL preparation split and habu2.f ENGINE-EMIT entry. Host readers retain host ABI. Freeze target declaration/code/signature membership, persist registry DATA before final D1 (USIGS always allots), prepare once and copy immutable owned capture. Then reopen native compiler session and compile source-bound target emitter outside capture. Pass owned capture explicitly, no ambient AOT-BUF cross-instance access/new format/disk stage. Translate supported moved fixed slots by identity; refuse incompatible host. Caller-selected private output includes temp/smoke/promotion. Test generation1 growth/shrink actual cap/heap versus constants, forced registry growth, session reuse and capture bytes unchanged by writer compilation.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
