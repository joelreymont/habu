---
title: Auto-stage new boot-prelude files in build-fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:09:27.965859+02:00"
---

Hazard class hit twice on 2026-07-21 (structure-make.f landing): adding a NEW file to the boot run-prelude plus a reference to it from an EXISTING prelude file cannot land in one step, because the currently-installed engine re-reads the prelude per its OWN baked file list, which lacks the new file - the reference hits E-UNDEFINED and install dies (rc 70). The worker lane and the merge gate each had to hand-stage: bake the new file's rows first with no reference, install, then add the reference, install again. Build the capability into tools/build-fixpoint.f: during install, when the tree's assembly manifest lists prelude files the running engine's baked list does not know, load those files from disk in manifest order BEFORE re-interpreting the rest of the prelude (the manifest on disk is the authority; the baked list is only a cache of it). Then a new-file-plus-reference landing is a single ordinary commit. Regression: reproduce the structure-make shape (new prelude file + same-commit reference) against an engine predating it and prove one install now succeeds; prove the fixpoint stays byte-identical x2. Guard: files loaded this way must still pass the same checked/certify discipline as baked-list files.
