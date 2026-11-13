---
title: Compile a quotation that reads locals on Wasm and x86-64
status: open
priority: 3
issue-type: task
created-at: "2026-11-11T15:03:31.535536+02:00"
---

Problem: Joel's 2026-11-11 ruling (docs/forth-card.md "2 Effects, locals, quotations") lets a quotation read the locals of the word it is written in, T0 only, arm64 first, then Wasm, then x86-64, each refusing the form until its emitter and kernel carry it. Only src/arch/arm64/t0-arm64.f fills the closure operations (EMIT-ENV-OPEN-XT, EMIT-REC-CELLS-XT, :850-851); on Wasm a kernel file using the form fails the build with 'ncomp: cannot compile WITH' and E-T0-UNFILLED (-9592), an internal error, not a refusal naming the form. Shown by the root-facts-a-fold worker (2026-11-11): a private ROOT-SCOPE in src/core/include.f taking a quotation that read outer locals made test/file-objects.f's Wasm build fail; the factoring was removed, so ROOT-ENTER / catch / ROOT-LEAVE is written out in four places (WITH, WITH-BYTES, WITH-SUPPLIED, LOAD-RAW). Acceptance: the Wasm T0 emitter compiles a quotation that reads locals, then x86-64's; until each does, its build refuses the form by name; once both do, include.f's four scopes share one word. Files: src/arch/wasm/, src/arch/x86-64/, src/core/include.f. Verify: a fixture using the form built for --target wasm and run under bun; include.f's shared scope word in a Wasm build (test/file-objects.f). Depends: Wasm step 6 (e4cb9016) for Wasm, x86-64 (3ed6f45d) for x86-64. Ownership: none. Claim: unassigned.
