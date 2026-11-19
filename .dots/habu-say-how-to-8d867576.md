---
title: Say how to split a record too deep to make
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:57:24.534823+01:00"
---

Problem: a record whose MAKE effect passes EFFECT-DEPTH-MAX is rejected with DEPTH-SUGGEST$'s help (src/core/checker.f), which advises buffers and callbacks, the fix for a signature, not for a declaration. Acceptance: that rejection's help says to declare an inner part as a type of its own; a test pins the text. Depends: habu-reject-a-too-07b69417. Claim: unassigned.
