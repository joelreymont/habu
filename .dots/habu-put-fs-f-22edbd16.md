---
title: Put fs.f and fs-mutate.f words into package FS
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T15:35:44.985234+02:00"
---

forth-card.md §1: every module is a package named for its subject, never an operation; no prefix-namespaced globals. src/lib/fs.f has 105 and src/lib/fs-mutate.f 81 top-level FS-* / FS-MUT-* definitions outside any package, and fs-mutate.f's packages are named for operations (FS-NOREPLACE, FS-MKDIR, FS-COPY, FS-ATOMIC, FS-CLEANUP, FS-EXIT). Move every word into package FS under its unprefixed name, private unless another package calls it; merge the operation packages into FS; convert callers to FS:X. Renaming public words changes the public API: list the public renames for Joel before the lane. Found reviewing tim's fserr lane (nsxkswyo), which must add no new prefixed globals.

Joel 2026-10-29: "fix the naming" (authorizes the public renames). carl owns it; brief carl-dots/.out/carl-file/brief-fs-package.md; lane starts in .jj-ws.noindex/carl-fsname once tim's fserr (nsxkswyo, same files) lands. 242 words defined in the two files, 44 named outside by non-test code (those become FS:X public).

Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fsname (base master 5276a4a3, fserr landed).
