---
title: Judge an exit between create and does>
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:33:01.861150+01:00"
---

Problem: ': DF ( -- ) create exit does> ( -- n ) @ ;' then 'DF W1' and ': U ( -- n ) W1 ;': the load is E-MISMATCH at W1 (the clause never runs, so W1 is a plain created word), tools/check.f --verify-only is rc 0. docs/forth.md:912 asks for a create before any exit and says nothing of an exit between create and does>, after which the clause is dead. Acceptance: load and check reject it at one place with one record, and docs/forth.md says it. Depends: none. Claim: unassigned.
