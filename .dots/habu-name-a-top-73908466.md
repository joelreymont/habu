---
title: "Name a top-level family a part's name hides"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:26:04.899100+02:00"
---

Problem: inside package AQ with a part AQ:TQ, a STRUCTURE field typed by the top-level package TQ's public family fam (reached through `using TQ`) is rejected: 'in rec-make: unknown type 'TQ:fam' in signature', then 'bad structure declaration 'rec'', rc 70. Reproducer, one token per line as usual: `package TQ public ENUM fam alpha beta ;ENUM ;package`, `using TQ`, `package AQ package TQ ;package`, `STRUCTURE rec FIELD f fam FIELD n n ;STRUCTURE ;package`. The same file without the part loads rc 0. Cause: TDGEN-FAM-REF (src/core/sumtype.f) names the family by a package qualifier that is resolved again where the generated text is evaluated, a part first (CHECKER-RESOLVE:HEAD-ROW, src/core/checker.f); inside AQ no qualifier reaches the top-level TQ, so the generated text cannot name the family the source named. It can only reject, never bind wrongly. Acceptance: the reproducer loads rc 0 and rec's maker takes a TQ fam. Depends: none. Consumers: none in Maki. Claim: unassigned.
