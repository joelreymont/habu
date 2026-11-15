---
title: Close the remaining checker registrars to programs
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:43:13.070718+01:00"
---

Problem: a program still rewrites recorded checker facts. TRUST-USIG! and TRUST-USIG-SYM! (src/core/checker.f) are global, and so are the words registered by xt! into the checker's slots in checker.f, check-walk.f and check-hook.f (CHECKER-DEFER, CHECKER-EXPORT-DECL, CHECKER-USING, CHECKER-DEFCAST, CHECKER-LINEAR and the rest). The public table CHECKER-REG:DECLARATIONS (checker.f) holds TRUST-DECL-REJECTED?, TRUST-DECL? and DECLARED, so with a private 'CAST: >ROW? ( n -- [ ^u8 n ^u8 n -- bool ] )', ': TWO ( -- n ) 2 ;' then 's" TWO" s" -- n n" CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DECL-ROW-OFF + @ >ROW? execute drop' then ': USE ( -- n ) TWO + ;' loads and USE underflows at run time, rc 67. The checker's deferred cells can be re-pointed: ': ALWAYS ( ^u8 n -- bool ) 2drop 0 0= ;' then "' ALWAYS is TYPE-MAY-ARRIVE-XT" makes 'NEWTYPE path frob' end in E-THROW 7192, rc 67, where it is E-BAD-DECLARATION. Acceptance: no word, table or cell that registers or rewrites recorded checker facts is visible to a program (privacy only: a private section, no caller check); each probe is E-UNDEFINED at the name it uses, rc 70. Depends: habu-close-the-effect-baaa22ca. Claim: unassigned.
