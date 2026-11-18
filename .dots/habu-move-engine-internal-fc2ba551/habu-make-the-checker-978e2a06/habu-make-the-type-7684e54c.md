---
title: Make the type-family internals private
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.389185+01:00"
closed-at: "2026-11-18T16:25:31.209638+01:00"
---

Problem: the TFAM, TYPE-FIELD and CHECKER-DECL-FRAME internals in src/core/type-family.f listed in the parent are public, so a program reads and stores them (`99 TFAM:PF-COMMIT-N !` runs, rc 0). Acceptance: each is E-UNDEFINED outside its package; the engine files that call them take them by EXPORT. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tf.
