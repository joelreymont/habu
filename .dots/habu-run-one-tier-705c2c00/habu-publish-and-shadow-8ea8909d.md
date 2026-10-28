---
title: Publish and shadow from NEMIT
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.571283+02:00"
blocks:
  - habu-seal-t0-x86-a3f261d9
---

Problem: publish.f/shadow.f read NART emissions and NHOST serves only the tier-1 unit cache. Acceptance: umbrella section 2 publish.f/shadow.f changes; compiler.f publishes inside HOST-WORK before RETIRE; NHOST TAKE/PUBLISH-TAKEN/ABANDON-TAKEN and COMMIT-OWNED collapse to COMMIT + UNIT-NOTIFY; the WITH-UNIT observer deleted; the file-object path (tools/native-unit-capture.f, tools/native-unit-object.f, tools/native-definition-cache.f: whole-file reuse, habu-install-file-objects-2d6c9b17) is KEPT and rebinds its capture to NEMIT, it is not part of what goes; public effect changes as listed (NPUB:PUBLISH-PENDING/RAW ( -- ), PUBLISH-PENDING-DEFER/DOES ( n -- ), NSHADOW:TAKE/TAKE-DOES/NATIVE-ADDRS! lose the emission). Decide CALL-LOC by whether PUBLISHED-ROWS diagnostics (compiler.f:193-200) survive. Files: src/compiler/native/publish.f, shadow.f, host-publish.f, host.f, compiler.f, aot-shadow.f, and the capture reads in tools/native-unit-capture.f, tools/native-unit-object.f. Verify: compiler-session, native-file-* e2e, full arm64 suite. Depends: L2. Ownership: those files. Claim: unassigned.

L2b (jerry, .jj-ws.noindex/jerry-t0-l1 on mnpmzrsn): the seam owns the rows (UNIT+, CALL-ROW+, ADDR-ROW+); cells BYTES and PATCH replace CODE$ and the row readers; seam SEAL ( at ) and SEAL-UNPLACED file NEMIT (impl 0, loc = calling token offset, a quotation as ADDR-CODE); CALL/TAIL take loc. Stack order: seam, L2a (vnpwyyqm), L5a (mnpmzrsn), L2b.
