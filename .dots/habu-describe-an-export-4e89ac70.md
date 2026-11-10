---
title: Describe an EXPORT alias as the word it exports
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T22:12:42.384245+02:00"
---

Problem: SEE of an EXPORT alias of a word made by a create ... does> definer (package DTF public EXPORT DTE:KID, then SEE DTF:KID) prints 'kind colon' and 'effect none recorded', though the alias runs the child's body and has its effect. The interpreter's EXPORT copies only the DKIND:VAL kind bit to the alias record, so the alias has no DNAME-ALLOC and no stored facts text; src/habu/describe.f KIND$ reads the record's own flags. Found by the kind fold of stage 1 (habu-describe-a-published-501824c2). Acceptance: SEE of an EXPORT alias prints the kind and the effect of the word it exports, for a colon word, a constant, a variable, a created word, a does> child, a defer and an immediate word; describe.f reads them from where the engine and the checker hold them, with no second rule. Files: named by the root cause (src/habu/describe.f, the EXPORT path in src/habu/packages.f, the checker's EXPORT-META-SYM). Verify: test/describe-test.f cases for each kind of exported word. Depends: habu-describe-a-published-501824c2 landed. Ownership: unassigned. Claim: agent=bob workspace=.jj-ws.noindex/bob-describe-alias (root-cause hunt).
