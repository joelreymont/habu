---
title: "State what survives APP-IMAGE:SAVE for buffers and lifecycle hooks"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T21:32:19.124548+03:00"
---

Problem: a Maki native image restored from a warm capture segfaults deterministically in the first REFEREE:REFILL-CHECK it runs when the capture was taken after one referee use; a capture taken before any referee use restores and runs the referee fine (measured 2026-09-14 on the maki-pin engine 10d66991; three-line reproduction: define a word that runs the referee, run it once, APP-IMAGE:SAVE, run it in the restored image -> SIGSEGV rc 134). Maki's referee registers a lifecycle hook (IMAGE-LIFECYCLE:REGISTER) that disposes a BUF: buffer, re-initialises a JR: reader onto a create'd buffer and clears a temp dir at capture. Acceptance: docs/forth.md (or the lifecycle doc) states what a hook may and may not leave for the restored image -- whether a BUF:-disposed mapping may be re-initialised after restore, whether a JR: reader initialised before capture survives, what IMAGE-LIFECYCLE guarantees about ordering and about dynamic buffers -- and a Habu test captures after a BUF: dispose plus re-init and uses the buffer in the restored image; plus the doc fact that find-name, defined and [defined] are not in a built application image (docs/forth.md:1186 tells agents to use find-name) while get-current search-wl is. Files: docs/forth.md, the lifecycle library, a test. Verify: the Habu test; Maki's test/native_image.py warm-recapture step. Depends: none. Ownership: Habu maintainer (cedar). Claim: unassigned.
