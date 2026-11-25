---
title: "Judge a kept variable's own block, not the caller's"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T22:23:03.449318+01:00"
---

Problem: '[: parse-name drop ;] KV @ 0 TIE execute' is rejected at TIE while the other argument order (m2b) is accepted; QUOT-PAIR's keeps reads a block state the caller's own kept variable gave the instance (probes /tmp/jerry-p1/psd/c/m2.f, m2b.f; design /tmp/jerry-p1/br/pass-plan.md section 1). Acceptance: m2 and m2b get the same answer on load and check, and diag-position and xt-effect pass. Depends: the quotation-facts feature (beb96862). Claim: unassigned.
