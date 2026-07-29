---
title: Give layout.f and snap-lib.f package owners
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T21:34:38.637859+02:00"
---

Full context: the snapshot relocation step-2 work (preserved as WIP commit e50fb3ec in .jj-ws/habu-relocate-snapshot-region-752042fe) is blocked because tools/package-diff-lint.f rejects its edits to src/habu/layout.f and src/habu/snap-lib.f: those files define global constants with no package owner. The long-term-correct fix is to give each file a real package (short name, package-local tails, cross-package calls qualified), NOT an exact-path exemption like the one habu2.f received in commit a943eb40 - habu2.f is the engine trunk with hundreds of pre-package globals, while layout.f and snap-lib.f are small constant tables that CAN be packaged in under 30 minutes. Acceptance: package-diff-lint passes on a representative diff touching each file with no new exemption added; existing callers updated; engine rebuilds to fixpoint; gate-stdlib red-phase set unchanged. If packaging genuinely breaks the bootstrap ordering (constants consumed before the package machinery loads), record that proof in this dot and only then fall back to an exemption with the proof attached.
