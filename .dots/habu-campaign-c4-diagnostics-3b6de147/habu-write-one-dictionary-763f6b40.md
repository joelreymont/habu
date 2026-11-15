---
title: Write one dictionary-full record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:51:06.323963+01:00"
closed-at: "2026-11-16T04:07:33.721910+01:00"
---

Problem: three words write E-DICTIONARY-FULL with two texts (src/habu/interpreter.f PKG-DICT-FULL and DEF-ROOM, src/core/checker.f CHECKER-OVERLAY ROOM), and src/compiler/native/publish.f DOES-PROVE and IMPORT-PENDING-DOES throw the bare E-NPUB-PENDING when a does> clause has no record left. Acceptance: one REJECTION word writes the record for every dictionary-full case; a load and a check of a file that fills the dictionary print the same placed record, rc 77. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration, lands with C4).
