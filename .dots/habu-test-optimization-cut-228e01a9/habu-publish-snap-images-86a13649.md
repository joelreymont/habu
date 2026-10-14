---
title: Publish snap images through REPLACE-STAGED
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T14:20:20.577797+02:00"
closed-at: "2026-10-15T03:10:13.820834+03:00"
close-reason: Retire the implementation-only routing proposal. snap-lib reserves and registers its descriptor-owned sibling before writing, signs before rename, and tests close/replace cleanup. REPLACE-STAGED has a distinct counted-path fill boundary and rejects nesting; forcing the image writer through it adds no established outcome.
---

Problem (lane 294 r4-bfxstage, d4e78e7b): src/lib/fs-mutate.f REPLACE-STAGED now publishes every -o and engine install (reserve sibling, register in the exit registry, fill, rename, remove on throw or die), but src/habu/snap-lib.f STAGE/CLAIM keeps its own staging on RESERVE-SIBLING (writes through the descriptor and signs), a second publication scheme. Acceptance: the image writer publishes through REPLACE-STAGED (or the one shared word grows the descriptor form it needs), a die or throw during an image write leaves no sibling (seen failing first through a real snap), engine rebuilt (baked), g1 == g2, two-gen. Files: src/habu/snap-lib.f, src/lib/fs-mutate.f.

Review 344 adds: REPLACE-STAGED does not support nesting (src/lib/fs-mutate.f ~:703-705 says so; unenforced): its `tmp` points into the task band FS-MUT-ATOMIC-PATH, which an inner RESERVE-SIBLING overwrites, so the outer sibling stays on disk with its registry slot held until exit (probe $HOME/.cache/tmp/kestrel-r4-rev344/nested.f); a long-lived process would leak one slot per nested call. The image writer is the first fill that could nest: make the sibling path per call (copied out of the band) or refuse nesting by a named throw, before routing snap-lib through it.
