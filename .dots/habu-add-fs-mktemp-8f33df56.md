---
title: "Add FS:MKTEMP for temp files"
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T19:12:44.140315+02:00"
---

Joel 2026-10-28 (interview): FS:MKTEMP ( prefix -- path fd ) creates and opens a file exclusively (O_EXCL, as fs-mutate.f FS-ATOMIC:OPEN-EXCLUSIVE) under $TMPDIR with FS:MKTEMP-DIR's root check, mkstemp(3) semantics; registers no cleanup, as FS:MKTEMP-DIR registers none. Convert every caller that makes a temp file by hand from $TMPDIR (52 files read TMPDIR directly). Claim: agent=carl workspace=.jj-ws.noindex/carl-mktemp (lane A adds the word; lane B converts callers from a census).

Lane A landed c953b4d3 "Add FS:MKTEMP for temp files" (FS:MKTEMP ( ptr u8 n -- ptr u8 n fd ); FS-ATOMIC:OPEN-EXCLUSIVE removed). Lane B: workspace=.jj-ws.noindex/carl-mktempb, brief .out/brief-mktempb.md: census rows 1-5 (zip.f, zip-offset-test.f, test/compiler/session.f, jsonl-test.f, check-cleanup-child.f) and the gate-pool.f fallback root; rows 6-11 are fixed-name protocols and stay.
