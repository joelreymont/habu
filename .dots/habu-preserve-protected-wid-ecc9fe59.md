---
title: Preserve protected-WID add return
status: active
priority: 2
issue-type: task
created-at: 2026-07-16T21:43:08.153482+02:00
---

Full context: src/habu/habu1.f BPROTWIDADD calls LPROTWIDQ with BL without saving x30, so the nested call overwrites the primitive return address and replays the append tail until the protected-WID table fills. bootstrap/cg/forth.fs mirrors the bug. Fix: preserve and restore x30 around the nested membership call in native and recovery emitters; add focused duplicate-add and existing-member regressions that prove one append, no replay, correct return, and native/recovery/fixpoint parity. Verify exact focused engine/bootstrap tests, typed-local diff lint, host/filemap lints, and full owning gate.
