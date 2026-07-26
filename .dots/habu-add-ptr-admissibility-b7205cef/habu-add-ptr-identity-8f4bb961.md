---
title: Add pointer identity controls to linear suite
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.324000+02:00"
---

test/type-linear-suite.f only: accepted one- and two-layer pointer identity rows ( ptr opt<linear> -- ptr opt<linear> ) and the two-layer form certify, proving the transport negatives discriminate rejection of @ and ! from inadmissibility of the pointer rows themselves. Acceptance: controls certify on master; re-applying the m14 mutation still kills exactly the four transport negatives while the new controls stay green; suite rc=0; diff lints clean.
