---
title: Keep compiler tier stable during an active definition
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T17:07:16.410211+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.780356+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The compiler tier is still re-read per token, so a tier change inside a definition crashes rc 134"
---

Owner requested: Rowan tier lane. Independent Astra xhigh reviewer jit_span_review reproduced on frozen f115e7885d20, binary b469558ed4cec9609748d820d3df804414fbc7f51835d7e67084aa2c20938c50: `1 set-tier TRUSTED: SWITCH ( -- ) 0 set-tier ; immediate s" SWITCH" 0 parse-imm : OP ( -- n ) 7 SWITCH ; tier0-count@ . ' OP tier0-code? . OP .` prints0,0 then crashes rc134. EM-COMMENT re-reads mutable TIER-CELL per token; BSETTIER neither latches the definition tier nor rejects changing it during compilation. Acceptance: choose and enforce a coherent definition-tier lifetime, preserve deliberate between-definition selection and error recovery, add direct and modeled-immediate both-direction regressions, verify produced code/provenance and no crash. This is additional to repaired interval/rejected-emission holes, not a reason to weaken them.


Update2026-09-11 14:15 UTC: Owner confirmed Rowan, rowan-jit-nest after its nesting fix. Plan: latch selected compiler tier at colon entry; changes inside a definition apply to the next definition, every body token uses the latched tier. Both switching directions receive focused regressions. Independent jit_span_review final verdict NOT CLEAR until this and restored default are fixed.
