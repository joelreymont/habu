---
title: Type a constructor in a quotation from its input
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T14:28:40.373424+02:00"
---

Problem: a constructor inside a quotation body has no payload on its stack, so TFC-SEED (src/core/type-family.f) takes the parameters its payload carries from the enclosing definition's declared output: `ENUM flag up down ;ENUM : Q3E ( -- option<flag> ) 3 [: OPTION:SOME ;] execute drop construct flag up OPTION:SOME ;` is rejected ('`execute` takes flag; the stack holds n'). Counting every parameter a payload names as decided makes Q3E check but breaks XPGMKQ (test/type-ctor-suite.f:486) and WNST-OPT (:570), and tools/check-verify-core.f:552 dies with E-CONSTRUCT-WIDE, because an open parameter on a quotation's row is one cell. Acceptance: Q3E checks; XPGMKQ, WNST-OPT, type-ctor and type-decl pass. Depends: none. Claim: unassigned.
