---
title: Publish IR-CANON ceilings to its consumers
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T03:22:39.123944+02:00"
---

Full context: from agent irrender 2026-07-30 (commit b93c178c). IR-RENDER and IR-DIFF commit to the same working-set ceilings as IR-CANON, but each restates the numbers, so the three can drift: E-IR-RENDER-CAP and E-IR-DIFF-CAP are unreachable from a checked caller today only because IR-CANON refuses first with E-IR-CANON-CAP (proven in ir-render.f with a 300-byte-name fixture). Have IR-CANON publish its ceilings as named public constants and make render/diff (and IR-ENCODE's CELL-MAX if it is the same commitment - measure) derive from them, so a ceiling change is one edit and the downstream caps stay exactly the defense-in-depth bound of their own buffers. Mutation: raise the published ceiling with the consumers underived - the drift must be a compile-time or gate failure, not a silent divergence.
