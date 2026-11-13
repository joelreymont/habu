---
title: Drop the zero-divisor test
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.370535+02:00"
---

Problem: default code still tests the divisor: the kernel division rows and their (DIV-ZERO) helper (src/habu/habu1.f EMIT-DIV-ZERO, BDIV0? :1852-1871), T0's divisor rule (src/habu/t0.f:396-410, t0-emit.f:331-336), Wasm's DIVIDE-BODY (src/arch/wasm/link-routines.f:176-185) and kernel-words.f:124; tier 1's division (select.f DIV-ZERO-ENTRY, emit.f, host.f SEED-DIVISION) needs the helper. Once DIVISOR? stops excluding 0, FOLD2 would fold a literal `1 0 /` on the host. Acceptance: no zero test in default code; T0 refuses a literal zero divisor, and a literal `0 throw`, at compile time; -1 and MIN-N keep their answers; tests of the run-time refusal are off until habu-add-the-checks-a49d054d. Depends: habu-delete-tier-1-dd3d6b86. Claim: unassigned.
