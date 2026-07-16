---
title: "Checker: preflight parsing file loaders"
status: active
priority: 1
issue-type: task
created-at: "2026-07-15T18:32:53.381249+02:00"
---

Full context: the checker already has the right semantic split: raw CHECK-CANDIDATE! rejects the unmodeled parsing immediates include/require while accepting the runtime stack loaders included/required/provided. The native compile path violates that boundary temporally: EM-COMPILE-CALL executes an immediate before the definition hook runs. In : MAIN ( -- ) 73 include /tmp/habu-fragment.f . ;, include evaluates `1 +` while the outer compiler has made the whole dictionary RX, so nested LCEMIT faults at str w9,[x28] (LLDB pc bin/hb+0x14098; direct --load rc 134). The same load path is otherwise fail-closed: : BAD ( -- n ) evaluate ; exits 70 with the named checker diagnostic. Implement a protected compile-immediate preflight installed by the checker and invoked before any source-defined immediate executes in a checked body. It must reject every unmodeled immediate before side effects, preserve audited TRUSTED: immediate bodies and PARSE-IMM modeled expansions, preserve included/required/provided runtime loaders, and leave top-level include/require unchanged. Render a stable structured unmodeled-immediate diagnostic and repair action. Add minimal direct --load regressions for include and require, positive runtime-loader and modeled-immediate regressions, recovery/bootstrap parity, snapshot-safe hook persistence/rebasing, typed gates, and exact full gates. Do not hardcode loader spellings in the compiler, ban runtime loaders, or patch include.f around the fault; no SUMTYPE/PRODUCT syntax. Claim: agent=review_fs_atomic workspace=.jj-ws/habu-checker-preflight-sol.
