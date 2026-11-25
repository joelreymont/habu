---
title: Let the check read a constant made from a constant
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T14:04:46.739848+01:00"
---

Problem: src/core/structure-decl.f:935 COUNT-LATER? leaves a BUFFER byte count to the run when it names a constant whose source is not an integer literal, e.g. `3 cells constant HS  HS constant ROOM`, so the STRUCTURE and every word that uses it go unverified (W-CHECK-DEFERRED). Tender tenderd/zen-client.f:125 sizes `caller`'s BUFFER with `BUF:HDR-BYTES constant HDR-BYTES`, and BUF:HDR-BYTES (src/lib/byte-buffer.f:181) is a colon word that answers the constant HDR-SIZE. Acceptance: a constant whose source is integer literals, constants the check knows and the arithmetic words the kernel folds has its value in the check; a BUFFER sized by one is verified (a test checks it with --verify-only); BUF:HDR-BYTES is a constant. Depends: none. Claim: unassigned.
