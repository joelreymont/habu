---
title: Place sum declaration records at their tokens
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.797325+01:00"
closed-at: "2026-11-16T04:07:34.942693+01:00"
---

Problem: sumtype.f derive checks (:985-1016) throw at table names and a missing ;VARIANT end token is not mapped (TDECL-COPY? :236), so their records carry no line; a PRODUCT's missing end says ;VARIANT (:511). Acceptance: /tmp/jerry-genplace/fx/g2.f cases all place at a source line; the message names the opened closer. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
