---
title: "Stop the loader's policy rejections with 70"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:52:13.989379+01:00"
closed-at: "2026-11-16T04:07:33.918228+01:00"
---

Problem: src/core/include.f OUTSIDE-ROOT (:539) and the sealed policy record (:2343) stop with 107 after writing their record; src/core/checker.f OPEN-USING (:13160) throws ENGINE-ERROR:USING-BAD-NAME with no record; docs/forth-card.md :43, :218 and docs/forth.md :384, :405 name deleted ENGINE-ERROR constants. Acceptance: each stops with 70 after one record, or is deleted as unreachable; the docs name no deleted constant. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
