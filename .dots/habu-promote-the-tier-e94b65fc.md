---
title: Promote the tier-1 code probe to tools
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:01:30.246522+03:00"
---

Problem: the register-allocation lane wrote build/codeprobe.f (1.2 KB), a harness that dumps the tier-1 emitted code of seven bodies the scaling yardstick cannot reach (two with a call inside a live range) for before/after identity checks of a codegen change; it lives in the ignored build/ directory of .jj-ws/rowan-alloc and dies with the workspace. Acceptance: tools/codeprobe.f with a header stating the bodies and how to compare two engines, a docs/debugging.md line, and one gate case that runs it on bin/hb. Files: tools/codeprobe.f, docs/debugging.md, test/gate-stdlib-cases.f. Verify: the case on the root engine. Depends: none. Ownership: hazel. Claim: unassigned.
