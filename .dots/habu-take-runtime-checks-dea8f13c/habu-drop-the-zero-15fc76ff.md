---
title: Drop the zero-divisor test
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.370535+02:00"
---

Problem: default code still tests the divisor: the kernel division rows and their (DIV-ZERO) helper (src/habu/habu1.f BDIV0? :1678-1689, EMIT-DIV-ZERO :4551), T0's divisor rule (src/habu/t0.f:390-405, t0-emit.f:332-336), Wasm's DIVIDE-BODY (src/arch/wasm/link-routines.f:146) and kernel-words.f:121. Once DIVISOR? stops excluding 0, FOLD2 would fold a literal `1 0 /` on the host. Acceptance: no zero test in default code; T0 refuses a literal zero divisor, and a literal `0 throw`, at compile time; -1 and MIN-N keep their answers; tests of the run-time refusal are off until habu-add-the-checks-a49d054d. Depends: none. Claim: agent=worker workspaces=.jj-ws.noindex/jerry-div (run-time test), .jj-ws.noindex/jerry-divlit (literal refusal).
