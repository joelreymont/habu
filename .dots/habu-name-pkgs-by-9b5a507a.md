---
title: Name packages by subject, operations in words
status: open
priority: 2
issue-type: task
created-at: "2026-10-21T18:42:39.783642+03:00"
---

Problem: Habu packages compound a subject with an operation or slice, giving words like JSON-WRITE:OBJECT-START. Joel (2026-10-21): the package names the subject and the operation goes in the word: JSON:WRITE-OBJECT-START. docs/forth.md:176-185 (Packages) says public words are short domain words but does not say what a package name is.
Acceptance: (3) Census every other src package for the same defect (candidates: FS-COPY, FS-LIST, FS-LOCK, FS-MKDIR, FS-NOREPLACE, FS-ATOMIC, FS-TIMES, TREE-COPY, PROC-*, *-EMIT families) and fold each one the rule covers; record any kept compound with its reason in this dot before closing.
Files: docs/forth.md, every caller; census results decide further files.
Verify: Habu's required gates for the touched libraries and the build.
Depends: none. Deferred by Joel: do later.
Ownership: Habu maintainers; Zen (phil) only files the dot.
Claim: unassigned.
