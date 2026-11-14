---
title: "Check FUNCTION:'s effect and form with the load's rules"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:36:25.858387+01:00"
---

Problem: src/habu/verify-source.f reads a FUNCTION: effect with its own reader (FFI-UNREAD), so tools/check.f --verify-only does not reject a second output, a type the declarer rejects ('( fd -- n )'), ptr without u8 (also 'ptr i32') or a 17th input, stops '( ptr u8 -- n n ) 16 STRUCT-RESULT' at the clause instead of the effect, and leaves a clause outside a declaration, FUNCTION: inside an open one and ;FUNCTION with none open to the load. Acceptance: check.f prints the load's record for each, one text for both, as check.f tests. Depends: habu-print-the-load-2dad70bd. Claim: unassigned.
