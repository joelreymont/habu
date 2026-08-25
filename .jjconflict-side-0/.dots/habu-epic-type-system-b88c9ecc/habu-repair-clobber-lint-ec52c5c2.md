---
title: Repair clobber lint current emitter syntax
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:21.896534+02:00"
closed-at: "2026-07-13T12:43:07.218426+02:00"
close-reason: Implemented current LABEL@ parsing, scalable source loading, production census, machine contracts, x13 regression, and LPROTWIDQ preservation; reviewed and merged with full gates green.
---

Context: tools/lint/clobber-lint.f uses a fixed FB-CAP smaller than current src/habu/habu2.f and LABEL-OPEN? plus CALLEE? recognize only legacy LNAME @ LBL, and LNAME @ BL, token shapes, while current emitters use LABEL@ LBL, and LABEL@ BL,. The owning lint is red on file capacity, and after a cap bump its production census would be effectively empty so a clean verdict could be vacuous. Cause: source growth and tokenizer contract drift while the fixture retained the old spelling. Fix: use the shared scalable lint reader, parse current syntax and any intentionally retained legacy syntax, count discovered routine labels and calls from the authoritative emitter source list, fail closed on an empty or implausibly smaller census, and package the reusable analyzer. Acceptance: a current-syntax fixture with write x13, BL to an x13-clobbering callee, then read x13 is rejected; existing syscall x8 and LR fixtures stay red; a clean current-syntax control passes; the live lint reports nonzero routine and call counts with zero findings; exact lint-tools owning gate is green.

Claim: agent=clobber-lint workspace=.jj-ws/habu-repair-clobber-lint-ec52c5c2
