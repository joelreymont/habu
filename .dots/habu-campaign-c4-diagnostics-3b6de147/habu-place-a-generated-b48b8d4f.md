---
title: "Place a generated name's clash at its declaration"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.818676+01:00"
closed-at: "2026-11-16T04:07:35.079578+01:00"
---

Problem: TDPLAN-NAME+ (sumtype.f:1488) and TDECL-ADDR-FREE (:2229) throw at a generated name in TDGEN-BUF, so the clash record has no line. Acceptance: /tmp/jerry-c4/pclash/p.f under bin/hb gives one record at the PRODUCT's token with an = word: note, as the ENUM front does. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
