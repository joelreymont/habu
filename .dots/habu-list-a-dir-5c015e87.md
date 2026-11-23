---
title: List a directory through its descriptor
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:36:20.270160+02:00"
---

Problem: 95c39c4e deleted FS-LIST's caller-owned contexts and with them EACH-TYPED-AT, the only way to list a directory through a descriptor the caller opened; master lists by path only (EACH-TYPED src/lib/fs-list.f:199). Omabox walks its sync root through anchored descriptors (its src/local.f:1187 LISTED, src/stale.f:180 MATCHED), so a rename or symlink swap between opening and listing a directory cannot redirect the walk; listing by path loses that. Acceptance: a public FS-LIST word lists the entries of a directory given its open descriptor, with no context argument, leaving the caller's descriptor and offset untouched, a refused "." being E-FS-LIST-OPEN; FS-LIST's test lists a directory through its descriptor after the directory's path was renamed. Depends: none. Claim: unassigned.
