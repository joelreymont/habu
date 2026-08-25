---
title: replace seventy one-line TRUSTED forwarders with PPRIM rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.840499+02:00"
---

Problem: 134 TRUSTED: definitions in src/core, 70 of them one-line forwarders (enum-decl.f:115-151 with 22 duplicated verbatim in structure-decl.f:92-124, generated-declaration.f:711-725, structure-make.f:82-87) that the files themselves say must not exist once the target has a checker-recorded effect (structure-decl.f:85-90). Precedent: TYPE-FIELD-OWNER got PPRIM: rows (checker.f:6375-6386) and decl-event.f:50-55 dropped its forwarders. Acceptance: PPRIM: rows for the ~30 distinct targets (TFAM, SCHEMA-REG, TYPE-DECL), forwarders deleted, callers call the targets qualified; the package lint and the error-code lint green; src TRUSTED census printed before/after. Files: src/core/enum-decl.f, structure-decl.f, generated-declaration.f, structure-make.f, checker.f. Verify: the declaration suites and both diff lints. Depends: none. Ownership: declaration front ends. Claim: unassigned.
