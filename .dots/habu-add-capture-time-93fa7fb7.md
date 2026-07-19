---
title: Add capture-time forged-WID AOT mutation gate
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T02:34:09.901146+02:00"
---

Problem: master exercises the RESTORE-side forged owner-frame rejection (test/owner-wid-doctor.f BUILD-AOT-BAD/BUILD-AOT-MAL -> rc 82 'AOT owner frame corrupt') but NOT the CAPTURE-side owner-WID validation in src/habu/aot-capture.f (OWNER-VERIFY / OWNER-BAD-COUNT..OWNER-BAD-PACKAGE-DUP, :319-:397). test/owner-wid-build-forge.f is a COMPILE-TIME seal test (reopen BUILD-EXT, call sealed SET -> rc 70) and does NOT subsume the retired tip's test/owner-wid-aot-mutate.f, which mutated the in-memory AOT capture buffers (forged owner pub/pri/protected WIDs) via an OWNER-WID-CAPTURE:POST-HOOK/POST-ARM capture seam the current engine lacks, requiring each build to fail closed. Acceptance: add a minimal named tested capture-mutation seam to aot-capture.f (defer with fail-closed default, retired before the seed like SNAP-CLOSE-SEAM), port the owner/WID-specific mutation kinds (pub-zero, pub-sentinel, pri-sentinel, pri-alias, swap, cross, protected, owner-touch, base-wid, boot-wid, boot-protected), assert each build fails closed with the OWNER-BAD diagnostic. Files: src/habu/aot-capture.f, test/owner-wid-aot-mutate.f (new), new driver. Verify: mutated builds fail closed; test/gate-stdlib.f owner-wid suite. Depends: none. Ownership: src/habu/aot-capture.f OWNER-VERIFY. Claim: unassigned.
