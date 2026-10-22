---
title: Key and check whole-file entries
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.927513+03:00"
blocks:
  - habu-restore-the-whole-0d964797
---

Problem: no file-level lookup exists; per-definition dispatch decides reuse. Acceptance: FILE-KEY = engine, target profile, mode, canonical path, root, load kind, occurrence (nothing from preceding files); one hb-file-<hex>.hfo per file; validity is size+mtime then source hash, then NFILE-INPUTS:CHECK, then NUNIT-DEP:CHECK of the consumed rows; a hit installs the object in place of the file's source, a miss runs it under capture and stores; per-definition dispatch, DEFINITION-KEY/ENTRY-PATH/FIND-HIT/COLD-CANDIDATE/ADMISSION-REASON$, .hbd entries and NCOMP:DEFINITION-CACHEABLE? are deleted. Verify: native-file-state/types/stack/root e2e cold/warm/unchanged with identical executables. Design: docs/package-build.md "Keys and validity"; working notes ~/.cache/tmp/carl-file/design.md. Depends: codec dot. Claim: unassigned.
