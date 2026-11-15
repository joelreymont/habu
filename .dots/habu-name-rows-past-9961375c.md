---
title: Name rows past 26 in one effect
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:13:03.789620+01:00"
---

Problem: src/core/render.f RROW-CAP (26, :285) sizes RROW-KEY, so an effect naming more rows renders ? (:296), sets RROW-UNSAID, and REC-SIG leaves a sig-less word unrecorded with W-EFFECT-NOT-RECORDED (REC-REFUSE-WHY :1986, REC-REJECT-HELP :2005). The cap is Habu's own table and the row names' 26 letters. Acceptance: an effect names every row it holds (facts text names rows past the letters as type variables are named past theirs), the table grows, and RROW-CAP's reasons go. Depends: none. Claim: unassigned.
