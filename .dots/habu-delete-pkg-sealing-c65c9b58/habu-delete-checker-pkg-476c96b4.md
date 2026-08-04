---
title: Delete checker package-seal mirror
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:25.619817+02:00"
blocks:
  - habu-delete-native-pkg-d80850de
---

Delete checker policy that mirrors the retired reserved package-name wall. Remove CHECKER-SEALED-PKG?, E-EXPORT-SEALED, EXPORT-SEAL-GUARD, and the sealed-name branch in CHECKER-LBUF-NAME-GUARD. Replace sealed-package negatives in test/type-export-suite.f and test/export-package.f with production positives proving HB-ERROR:BAD-TAG can be exported and called. Owner and files: src/core/checker.f, test/type-export-suite.f, test/export-package.f. Depends on the native wall deletion and hard error rename. Acceptance before M17: exact absence census, positive export/call probes through real checker and compiler paths, and exact typed-local/package diff gates. No suite, replacement denylist, constructor protection changes, compatibility alias, new lint, or unrelated checker policy.
