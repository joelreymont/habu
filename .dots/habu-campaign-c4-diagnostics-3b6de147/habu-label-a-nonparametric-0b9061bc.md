---
title: Label a nonparametric effect at its token
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:47:38.625668+01:00"
closed-at: "2026-11-16T04:07:35.670191+01:00"
---

Problem: src/core/render.f DIAG-LABEL (:1582-1598) has no RF-NP arm (nor RF-REJECTED, RF-USHADOW, RF-UAMBIG), so ': LRD-FETCH ( ptr a -- n ) @ ;' dies 'render: a rejection with no label' rc 76; test/load-reject-diag-test.f:462-479 fails. Acceptance: one placed record, rc 70; every kind a writer gives has a label. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
