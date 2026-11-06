---
title: "C2c-f: find why facts text writes ? past the renderer's limits"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T01:33:35.694895+02:00"
closed-at: "2026-11-07T04:02:42.527116+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: the C2c candidate (/tmp/jc2.dP/hb) writes `?` (render.f EMIT-UNSAID) for a type nested ten deep (test/engine-stack-wide.f:158-159 ITEM, `sw-plus<sw-double<...<?>>>`; reproducer /tmp/jrq.sa/deep.f) and for a 24th type variable (test/c2-retained-contract-wide.f C2V-DROP), and refuses them with `facts text differs` (suites engine-stack-wide, c2-view-record-source). Acceptance: a reported cause with file:line for each limit, whether facts text can lift it, and the rule the fix follows; the fix is a second brief, a commit under nkwlmvos. Files: read-only. Verify: /tmp/jrq.sa/deep.f on the candidate. Depends: none. Brief: /tmp/jerry-big/brief-rca-q.md. Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rca-q.
