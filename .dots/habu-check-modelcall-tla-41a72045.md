---
title: "Check ModelCall.tla's resting sends with TLC"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:40:18.444762+01:00"
---

Problem: 632e1946 'Rest a failed model send and send it again' rewrote spec/ModelCall.tla (814 lines: resting, RESEND-WAIT!, TIER-RESEND!, the CANCEL landing) and added spec/ModelCall-rests.cfg, and landed without a TLC run, since a run needs Joel's approval (CLAUDE.md, Quality); its review judged the checked states unchanged, which no run has shown. Acceptance: with Joel's approval, TLC checks spec/ModelCall.tla under ModelCall.cfg and ModelCall-rests.cfg with no violation, or each violation is dotted with its trace. Depends: Joel's approval of the run. Claim: unassigned.
