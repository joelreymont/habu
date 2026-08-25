---
title: CUDA-SCOPE public frame open/close API
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T07:41:44.428411+02:00"
---

Enhancement identified by the maki migration (stack cb1e4cae): gpu.f's cross-call SETUP/RELEASE boundary uses the top-level ledger + manual UNWIND, which assumes no other code runs a bare UNWIND between the calls (true today). A public OPEN-FRAME/CLOSE-FRAME (or commit/detach on SCOPE) would make cross-call owners full RAII like the one-shot scopes and remove the ambient assumption. lib/ptx/cuda-scope.f + injection-matrix extension; superseded eventually by linear owner types (standing note).

Note 2026-07-21 (WITH-BYTES landing b1044d80): when this frame API happens, also consider unifying the 6-line primary-error-wins combinator duplicated between cuda-scope (COMBINE, checked) and lib/memory.f (WB-COMBINE, trusted) - the WITH-BYTES lane deliberately did not share it because the trust surfaces differ; a unification must not widen cuda-scope trust.
