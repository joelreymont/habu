---
title: Stamp sources at the read
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:48:18.141666+02:00"
closed-at: "2026-10-30T19:25:13.613092+02:00"
close-reason: "landed uptrmpqo 9950bd61: STORE-OWN stamps, then hashes disk; kind 0 when it differs from the bytes read. Opus review ACCEPT after the regression case (native-file-inputs-format.f, red on the old STORE-OWN). Tests: native-file-inputs-format, native-file-tree-share-e2e rc 0."
---

Found by the U5 review (read, not reproduced). The source row's stamp is taken at OBJECT-OPEN (tools/native-definition-cache.f:1334-1338), after the file was read (:1885-1900). The digest covers the bytes that ran, but the stamp is the file's ctime at OBJECT-OPEN. An edit landing between the read and the stamp records the new ctime beside the old digest; the next build skips hashing (STAT= holds) and reuses stale code. mtime had the same gap. verify-source.f's QUIET-READ already captures the stat before and after the read; the row should carry the stat taken with the bytes that were read (refuse to store, or stamp nothing, when it moved during the read). Reproduce first: an edit injected between read and OBJECT-OPEN, then a warm build that must recompile.

Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fstamp brief=.out/carl-file/brief-stamp-read.md
