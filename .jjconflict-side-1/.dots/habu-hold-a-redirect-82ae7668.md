---
title: Hold a redirect to one declared effect
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T09:46:40.043011+02:00"
---

Full context: src/compiler/native/reach.f REDIRECT holds the two words to one declared stack effect only when the checker still has both, and it does not for the interesting subject: a word that came out of the sealed image has no row in the effect store (measured: EFFECT-QUERY answers false for SYM-FOLD-C, TAG and SYM-STR=CI, while a word defined in the same process answers its din and dout). So a redirection of an image word rests on the same-tail rule and on the caller's statement that the two definitions are the same definition. The missing capability is a migrated definition's ACCEPTED effect kept beside its published routine, so the seam can hold the new routine against what the old code's callers were compiled around rather than against a store that no longer remembers. Depends: src/compiler/native/reach.f EFFECT-CK, src/compiler/native/publish.f (the log is where a per-routine fact belongs), dot habu-bind-checker-env-ed4f9f87. Ownership: unassigned. Claim: unassigned.
