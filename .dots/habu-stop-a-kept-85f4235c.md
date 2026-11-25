---
title: Stop a kept pair widening a pointee term
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T22:23:03.459396+01:00"
---

Problem: QUOT-PAIR's kept arm writes has into got (has got Q-FACTS+), which can widen a pointee term handed out by @, so a later keeping judged on that term reads facts given after it (same family as m1; /tmp/jerry-p1/br/pass-plan.md section 9). Acceptance: a program where a kept store's got side is a pointee term from KV dup @ and a later parsing quotation reaches it is rejected on load and check alike; diag-position and xt-effect pass. Depends: the quotation-facts feature (beb96862). Claim: unassigned.
