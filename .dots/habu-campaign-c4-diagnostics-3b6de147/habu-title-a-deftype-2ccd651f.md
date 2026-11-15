---
title: Title a DEFTYPE without a base as a deftype
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T02:38:47.476322+01:00"
closed-at: "2026-11-16T04:07:33.850559+01:00"
---

Problem: src/core/sumtype.f TDECL-DEFFAMILY titles every record 'newtype', and TDECL-DEFTYPE and verify-source.f RECORD-DEFTYPE reach it: 'DEFTYPE DUPY' twice prints 'bad newtype declaration'. Acceptance: the load and check.f title a DEFTYPE's records 'bad deftype declaration'. Depends: none. Claim: agent=W158 workspace=.jj-ws.noindex/jerry-w158
