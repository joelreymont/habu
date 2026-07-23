---
title: Shorten boundary policy
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T03:48:56.798207+02:00"
blocks:
  - habu-use-hook-identities-731c89b0
  - habu-shorten-boundary-diagnostics-b0c3fa47
---

Why: hook and definition classification plus its finding handlers still carry obsolete UB-prefixed names after registry authorization and diagnostic cleanup. Exact result: in tools/checked-boundary-lint-core.f rename the remaining private declarations from UB-SET-CHECK-OFF? through UB-HOOK-NAME? and from UB-HANDLE-INSTALL through UB-HANDLE-COLON by dropping only UB- and updating all references. The registry leaf has already deleted both legacy allowlist predicates; do not recreate them. Preserve exact recognition roles for set-check, set-top-check, tick and bracket-tick ordering, preflight install, checker on/off transitions, TRUSTED, KERNEL, CHECKED, plus-colon, colon, semicolon, anonymous definitions, nested definition state, and finding dispatch. Forbidden: new policy, substring tests, name-only authorization, public words, aliases, or parser duplication. Pre-change proof: the owned policy and handler declarations retain UB prefixes. Acceptance: every structural hostile fixture and all current hook rows keep the registry leaf outcomes; ordinary checked definitions remain clean; the real command and CBLT suite are byte-identical; no UB-prefixed declaration in these policy slices remains; exact ownership/type checks pass. Files: tools/checked-boundary-lint-core.f. Depends: habu-use-hook-identities-731c89b0 and habu-shorten-boundary-diagnostics-b0c3fa47. Ownership: CHECKED-BOUNDARY-LINT private policy and handler names only. Claim: unassigned.
