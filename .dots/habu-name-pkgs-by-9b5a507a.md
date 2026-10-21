---
title: Name packages by subject, operations in words
status: open
priority: 2
issue-type: task
created-at: "2026-10-21T18:42:39.783642+03:00"
---

Problem: Habu packages compound a subject with an operation or slice, e.g. src/lib/json-read.f:64 package JR, src/lib/json-write.f:36 JSON-WRITE, src/lib/json-copy.f:8 JSON-COPY, giving words like JSON-WRITE:OBJECT-START. Joel (2026-10-21): the package names the subject and the operation goes in the word: JSON:WRITE-OBJECT-START. docs/forth.md:176-185 (Packages) says public words are short domain words but does not say what a package name is.
Acceptance: (1) docs/forth.md Packages paragraph gains the rule: a package names a subject, never an operation on it or a slice of it; files that read, write or copy one subject reopen that subject's package and put the operation in the word (JSON:WRITE-OBJECT-START, not JSON-WRITE:OBJECT-START); never compound a package name from a subject and a verb; resolve a tail collision in the word name, not with a new package. (2) JR, JSON-WRITE and JSON-COPY fold into one package JSON with word renames resolving collisions; JSON-RPC and JSONL are separate subjects and stay. (3) Census every other src package for the same defect (candidates: FS-COPY, FS-LIST, FS-LOCK, FS-MKDIR, FS-NOREPLACE, FS-ATOMIC, FS-TIMES, TREE-COPY, PROC-*, *-EMIT families) and fold each one the rule covers; record any kept compound with its reason in this dot before closing. (4) All Habu callers updated in the same change; downstream renames listed in the commit for consumers (zen, tender, maki: about 26 files use JSON-WRITE:).
Files: docs/forth.md, src/lib/json-read.f, json-write.f, json-copy.f, their tests, every caller; census results decide further files.
Verify: Habu's required gates for the touched libraries and the build; rg shows no remaining JR:, JSON-WRITE:, JSON-COPY: in Habu.
Depends: none. Deferred by Joel: do later.
Ownership: Habu maintainers; Zen (phil) only files the dot.
Claim: unassigned.
