---
title: Select the tier with 1 set-tier in the seven require-aot-mode suites
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T07:56:18.190793+03:00"
---

Problem (found by the tier-prepend lane): seven suites select tier 1 by 'require test/compiler/aot-mode.f' - test/compiler/native-trap.f:4, native-string.f:3, native-rename-rows.f:4, test/address-cell-index-recovery.f:2, test/image-lifecycle-tasks.f:5, test/address-cell-index.f:2, test/address-cell-tasks-subject.f:2 - although docs/gate.md's rule (landed 2716d8bd) is 1 set-tier before the requires with a one-line reason, and aot-mode.f's header now says it is for a caller that runs one unchanged file at both tiers; require is once-per-image and keyed by path, so the line is a silent no-op in an image that already loaded it. test/gate-stdlib-cases.f:217 also still lists aot-mode.f before native-tape-owner.f, which selects the tier itself since 2716d8bd. Acceptance: the seven say 1 set-tier with their reason; the tape-owner row lists only its file; rg -n 'require test/compiler/aot-mode.f' test finds nothing; each changed suite green standalone; test/run.f green. Files: the seven, test/gate-stdlib-cases.f. Depends: none. Ownership: hazel (gate/test load path). Claim: unassigned.
