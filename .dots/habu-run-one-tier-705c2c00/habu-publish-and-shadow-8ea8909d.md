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

Carried from the L2b review (T0-EMIT SEAL, src/habu/t0-emit.f): (1) SEAL files every call row with implementation 0, where tier 1 files `NHOST:ID-OF target`; host-publish EDGE-TARGET and native-unit-object.f DEF-ADDR-REF's known flag (:1762) read that implementation, so publishing a T0 emission needs the real id or a reader that does not need it. (2) arm64 ADDR-DATA rows carry origin -1, which native-unit-object.f DEF-ADDR-REF refuses (`owner 0 < if BAD`); settle the origin before T0 publishes on arm64.

Carried from habu-clear-held-defer-0ff5391d: src/compiler/native/publish.f PUBLISH-PENDING-DEFER sets HELD-DEFER before COMMIT-OWNED, and COMMIT-OWNED runs NHOST:TAKE outside its `finally`; a throwing TAKE leaves HELD-DEFER set and the next ordinary publish commits as a defer. The collapsed COMMIT must clear its held state on every exit.
