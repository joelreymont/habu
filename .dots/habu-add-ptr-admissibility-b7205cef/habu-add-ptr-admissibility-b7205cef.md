---
title: Add pointer admissibility controls
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.180087+02:00"
blocks:
  - habu-add-ptr-identity-8f4bb961
---

Why: TC21-TC28 in test/type-linear-suite.f do not prove that ptr-of-generic-linear signatures are admissible; a blanket rejection would keep every transport negative green, so the pins do not discriminate @/! transport rejection from pointer-row inadmissibility. Result: the child adds accepted one- and two-layer identity rows, ( ptr opt<linear> -- ptr opt<linear> ) and its two-layer form, beside the negatives. Owner: test/type-linear-suite.f only. Acceptance: both controls certify on current master; reapplying the m14 mutation still kills exactly the four transport negatives while the controls stay green; the real type-linear suite plus typed-local and package diff gates pass.
