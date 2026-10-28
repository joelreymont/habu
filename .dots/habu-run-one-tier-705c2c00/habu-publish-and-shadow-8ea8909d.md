---
title: Publish and shadow from NEMIT
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.571283+02:00"
blocks:
  - habu-seal-t0-x86-a3f261d9
---

Problem: publish.f/shadow.f read NART emissions and NHOST serves only the tier-1 unit cache. Acceptance: umbrella section 2 publish.f/shadow.f changes; compiler.f publishes inside HOST-WORK before RETIRE; NHOST TAKE/PUBLISH-TAKEN/ABANDON-TAKEN and COMMIT-OWNED collapse to COMMIT + UNIT-NOTIFY; unit-cache readers and the WITH-UNIT observer deleted; public effect changes as listed (NPUB:PUBLISH-PENDING/RAW ( -- ), PUBLISH-PENDING-DEFER/DOES ( n -- ), NSHADOW:TAKE/TAKE-DOES/NATIVE-ADDRS! lose the emission). Decide CALL-LOC by whether PUBLISHED-ROWS diagnostics (compiler.f:193-200) survive. Files: src/compiler/native/publish.f, shadow.f, host-publish.f, host.f, compiler.f, aot-shadow.f, tools/native-unit-capture.f, tools/native-unit-object.f. Verify: compiler-session, native-file-* e2e, full arm64 suite. Depends: L2. Ownership: those files. Claim: unassigned.
