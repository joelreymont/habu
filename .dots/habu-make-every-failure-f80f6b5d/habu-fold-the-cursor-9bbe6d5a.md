---
title: Fold the cursor-line copies into one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.129794+02:00"
---

Problem: src/habu/interpreter.f LINE (:326-328) and REFUSAL's guard (:332, `SRCLOC:FILE-BUFFER? if LINE else 0 then`) compute what src/core/checker.f SRCLOC:LINE (:29464-29467) answers: the cursor's line from the open file's first owned byte, 0 outside file text. src/compiler/native/definition.f:459-461's copy goes with tier 1. checker.f NAME-LINE's cursor fallback (`p BYTE-LINE`, :29538) under REFUSE-DUPLICATE's guard computes it too. Acceptance: REFUSAL and REFUSE-DUPLICATE's cursor case call SRCLOC:LINE and interpreter.f's LINE is gone; the chain build reaches its fixpoint and the suite passes. Depends: none. Claim: unassigned.
