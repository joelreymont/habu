---
title: Make the name follow its redirected callers
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T09:46:56.676557+02:00"
---

Full context: src/compiler/native/reach.f REDIRECT moves every call instruction that entered a word's old code onto the routine the chain published, and leaves the OLD dictionary record alone. So after a redirection the word's existing callers run the new routine while a definition compiled later and naming that word still gets the old code - correct code, just not the new code. Repointing the old record is the publication seam's business (src/compiler/native/publish.f RETARGET), and it refuses the interesting subjects today: an engine-internal word is refused by FLAGS-CK because the interpreter will not enter one, and the checker's own hot words are all marked internal at seal time (src/core/internal-mark.f; measured: SYM-FOLD-C, TAG, PAY and T-RES-WALK all carry DNAME-INT). Needed: a seam entry that points a second record at a routine this process published, with the internal-word rule re-derived for a record whose callers are compiled code rather than the interpreter. Depends: src/compiler/native/publish.f, src/compiler/native/reach.f. Ownership: unassigned. Claim: unassigned.
