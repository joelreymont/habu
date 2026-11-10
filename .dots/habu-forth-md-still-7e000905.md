---
title: forth.md still describes an unchecked 0 set-check window
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:08:06.982903+02:00"
---

Problem: docs/forth.md:36-38 says a text that runs 0 set-check leaves every later definition and token unchecked, :1533-1535 says it makes : the plain native colon, and :2226-2229, :2558-2566 and :2576-2577 describe the same window; since 19442634b0 (Call the checker from the interpreter by name) src/habu/outer.f:467 calls CHECK:TOP-LEVEL on every event and no window exists (probes p6, p19 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/card/ex). The card's pointers at docs/forth-card.md 'lists the open cases' (§5) and §10 'what evaluate-closed leaves open' lead to the stale passage. Found by the review of habu-cut-the-card-6526717c; the last three passages were not tested one by one. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each passage states what bin/hb does, shown by a probe, and the card's two pointers lead to true text. Files: docs/forth.md, docs/forth-card.md. Verify: the probes rerun. Depends: habu-cut-the-card-6526717c landed (same file). Ownership: the docs. Claim: unassigned.
