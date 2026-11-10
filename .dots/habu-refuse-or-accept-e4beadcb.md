---
title: Refuse or accept alike two type names differing only in case
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:29:03.071523+02:00"
---

Problem: type names compare case-sensitively (CT-FIND src/core/checker.f about :2376-2384; TFAM-NAME-MATCH? src/core/type-family.f about :576-577) but a build files each type as an entry in a case-insensitive wordlist (OUTER:ALIAS-RECORD through WL-PROBE, src/habu/packages.f about :421-436). So `--load` accepts `DEFLINEAR P:a` and `DEFLINEAR P:A` (and two families so named) while a build throws 'duplicate definition' at the second entry. From reading (review of lane kukntvwk), not run. Acceptance: a load and a build give the same answer for two linear types and for two families whose names differ only in case, with one rejected program in the suite if the answer is a refusal. Files: src/core/checker.f, src/core/type-family.f, src/habu/packages.f, src/habu/linker.f (TYPE-ENTRIES). Verify: the two programs by --load and by hb-build. Depends: none. Ownership: those files. Claim: unassigned.
