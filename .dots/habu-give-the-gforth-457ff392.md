---
title: Give the Gforth mirror a package-gate category
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:04:43.317697+02:00\""
---

Full context: measured by agent stage0using 2026-07-30. tools/package-diff-lint.f has no category for bootstrap/cg/forth.fs, so EVERY edit that touches an existing definition in the Gforth mirror fails the gate - adding one trailing comment to the existing global BCOUNT reports E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3. The mirror runs under gforth, which has no habu package word, so packaging its definitions is IMPOSSIBLE in that file - a structural impossibility, not a debt like habu2.f. Decide and implement the principled category: the mirror path admits changed and new definitions (mirror discipline is owned by the parity gates in tools/bootstrap-codegen-test.f and bootstrap-mirror-lint.f, not by package scope), with the same one-comparison-site row pattern the engine-trunk category uses, pinned both ways in tools/package-diff-lint-test.f (a mirror edit passes; the same edit at a non-mirror .fs path still fails). Depends on the admission-key rework in flight in .jj-ws/habu-relocate-snapshot-region-752042fe (dot habu-admit-layout-f-7e317a72) - build on top of it, not beside it. This unblocks landing the stage0 using commit b9d5fca5 (44 findings today, 40 of them this pre-existing gap).

Claim: agent=mirrorcat workspace=.jj-ws/habu-give-the-gforth-457ff392
