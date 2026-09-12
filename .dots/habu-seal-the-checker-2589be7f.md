---
title: "Seal the checker's package-verify window words as internal"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T12:48:02.935619+03:00"
---

Problem: src/core/checker.f:816-818 states CHECKER-VERIFY-PKG-START and CHECKER-VERIFY-PKG-DONE stay checker-internal, but a file containing only CHECKER-VERIFY-PKG-START runs and exits 0 at top level (measured 2026-09-12): src/core/internal-mark.f IMK-MARK marks a global colon record internal only when SIG-MIN-IN is negative, and both carry zero-cell PRIM: rows (checker.f:6497-6500, added so the native compiler compiles the call window from its TRUSTED caller), so SIG-MIN-IN is 0; CHECKER-RESET-SOURCE is internal only through the explicit REG-PROTECT at checker.f:13328. test/compiler/ir-id.f F64/F66/F67/F68/F70/F71 assert the refusal. Acceptance: the two words are refused at top level by name like CHECKER-RESET-SOURCE (an explicit REG-PROTECT, or the marking pass treating trusted-only PRIM rows as unknown), the compiled TRUSTED callers still work (the native compiler suites), and ir-id's cases are green. Files: src/core/checker.f, src/core/internal-mark.f, test/compiler/ir-id.f. Verify: test/compiler/ir-id.f, the native compiler suites, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
