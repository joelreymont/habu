---
title: Mark non-returning callees by capability, not address
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T04:03:53.828870+02:00"
---

publish.f ENDS-PROCESS? excludes the shared trap routine from the clobber-coverage rule by comparing the callee's resolved address against NTRAP:ROUTINE$'s (merged 7b7db6c2). Sound today - one non-returning routine exists tree-wide and nothing it destroys is observable - but it is an address-identity check where the meant fact is a CAPABILITY of the callee ('this routine never returns'). The moment a second non-returning routine exists the comparison silently under-covers. Move the fact onto the callee's record (the checker already certifies CTL-DEAD; the publication record is where the seam reads callee facts) and make ENDS-PROCESS? read it. Acceptance: two distinct non-returning routines both excluded; a returning routine at either address not excluded; fixture through the real seam. Files: src/compiler/native/publish.f, the record it reads. Depends: none (before any second trap-like routine is minted).

CAPABILITY NOW EXISTS 2026-08-10: HIR-WORD:CALLEE-DEAD? (merged c9c987b8,
dead-path lane) reads the checker's control record through the word model -
exactly the fact ENDS-PROCESS? should consume instead of comparing the
callee's address against NTRAP:ROUTINE$'s. The fix is now a read of an
existing fact, not new recording.
