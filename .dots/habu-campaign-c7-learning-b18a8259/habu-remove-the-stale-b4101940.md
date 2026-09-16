---
title: Remove the stale CAD-EFFECT section from docs/effects.md
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T14:29:12.945654+03:00"
---

Problem: docs/effects.md's 'CAD semantic effect vocabulary' section describes src/cad/effect-types.f, src/cad/effect.f and a lib/nominal substrate, and E-CADEFF-*/E-NOM-* codes, none of which exist in Habu or in Loom (verified 2026-09-16 during the tracker rebuild; three dots died on it). A reader learns a vocabulary that does not exist. Acceptance: the section is removed or replaced by one paragraph stating that CAD effects moved to Loom with a pointer, every remaining word named in docs/effects.md resolves in the tree (rg each backticked name), and the effects-grammar fixture sweep in the gate still passes. Files: docs/effects.md. Verify: rg for the removed names returns nothing outside docs/archive; bin/hb --load test/run.f green. Depends: none. Ownership: docs lane. Claim: unassigned.
