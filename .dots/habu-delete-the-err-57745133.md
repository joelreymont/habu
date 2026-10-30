---
title: Delete the error codes no code throws
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:50:32.951981+02:00"
---

Problem: src/lib/errors.f keeps constants nothing throws: -3506 E-FFI-CALLBACK-FULL (errors.f:161, whose comment said retired and retained for numeric compatibility) and the E-PTXTC-* codes (errors.f:144 names TC-ARCH!, a word that does not exist); found by the src/lib comment review, and Habu keeps no legacy codes. Acceptance: the unthrown constants and their comments are gone; every remaining E-PTXTC-* and E-FFI-* code has a throw site. Files: src/lib/errors.f. Verify: rg -n 'E-FFI-CALLBACK-FULL|E-PTXTC-' src tools test prints only throw sites or nothing; hb --load tools/check.f -- src/lib/errors.f rc 0; hb --load tools/error-code-lint.f green. Depends: habu-cut-comments-in-6b103fd8 (the lib comment lane lands first). Ownership: src/lib/errors.f. Claim: unassigned.
