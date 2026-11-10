---
title: Drop header stores from the run rule
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T11:29:37.951686+02:00"
closed-at: "2026-11-10T23:38:05.006463+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: src/habu/linker.f WALK-STORERS :888 walks a STORERS pair held by header data (-1) in every walk, so every file that ran a word depends on every file that stored into header data; a word run during a build reads hb's own header cells (stores redirected by ROUTE-HEADER habu2.f:8270-8275, reads not). One unused word in src/habu/repl.f compiles 31 files for this alone. Acceptance: a header storer is reached only when a reached piece or the running piece holds a header-data address outside its STORES rows; the repl.f edit compiles 9 files, not 40; unsigned warm equals cold; test/file-objects.f B2 compiles 3. Files: src/habu/linker.f, test/file-objects.f. Verify: brief .jj-ws.noindex/carl-layout/.out/briefs/lane-header-store-no-run-dep.md Evidence. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-hdr
