---
title: Relocate quotations stored in typed image data
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.371249+03:00\""
closed-at: "2026-09-10T20:37:34.459158+03:00"
close-reason: Normal typed quotation stores now register persistent DATA cells with the shared relocation table while transient buffers remain unmarked. Native stored-quotation and combined app-image tests pass, including overwrite, source-free restore and recapture.
---

Owner: Cedar; coordinate checker memory facts, compiler store lowering, typed storage and existing SNAP-RELOC records. Real failure from app-image agent: [: INCREMENT ;] stored via normal ! in TYPED-VARIABLE ACTION [ n -- n ] executes before capture but remains at the old code address after restore, causing SIGILL near 0x1961c20 in two images with different region bases. Existing xt! declares persisted code cells; normal typed quotation stores currently do not. Fix at the shared storage/relocation boundary, including overwrite and repeated capture, without per-application marking workarounds or treating arbitrary numbers as pointers. Dynamic callback storage must remain usable: xt! currently rejects destination cells outside DATA, so blindly replacing every ! with xt! is wrong. Acceptance: typed stored quotations execute after first and second source-free image restore; transient typed quotation buffers still work. Reproducer test/app-image-subject.f in paused app revision 4c58d6dd. Related existing dots habu-declare-persisted-cb-b150b5d5 and habu-add-a-quotation-1610f30c.
