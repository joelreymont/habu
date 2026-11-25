---
title: "Judge a top-level step in the check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:33:01.876433+01:00"
---

Problem: tools/check.f --verify-only accepts a top-level input underflow ('DF W1' on an empty stack, DF a definer that takes a cell) a file that leaves cells ('5' alone) and a type a step takes wrongly ('5 KV !' with KV of a quotation); the load rejects each (E-INPUT-UNDERFLOW, E-EVAL-RESIDUE, E-TYPE). Acceptance: check answers as the load at the load's place, or its docs say which top-level steps it leaves to the run and it warns there. Depends: none. Claim: unassigned.
