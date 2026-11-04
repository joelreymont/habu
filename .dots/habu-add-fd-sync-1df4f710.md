---
title: "Add FD:SYNC and FD:RESIZE for Maki's file store"
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T14:02:42.169325+02:00"
---

Problem: Maki's file store (tim, 2026-11-04) must sync an upload and its directory before the PostgreSQL row naming it commits, and after a crash cut a file back to its last committed part. Habu binds no fsync (Zen binds it privately, zen/src/original-store.f:18); ftruncate is bound privately at src/lib/fs-mutate.f:355. Acceptance: public FD:SYNC ( fd -- ) over fsync and FD:RESIZE ( fd n -- ) over ftruncate in src/lib/fd.f, each E-FS-IO on refusal, RESIZE E-SPAN-LENGTH on a negative length; fs-mutate's PREPARE-DST calls FD:RESIZE and its TRUNCATE-CALL goes; fd-test cases for cut-then-append, a directory sync and the refusals. Decision: DECISIONS.md 2026-11-04 'Maki's file store'. Files: src/lib/fd.f, src/lib/fd-test.f, src/lib/fs-mutate.f. Verify: bin/hb --load src/lib/fd-test.f and src/lib/fs-mutate-test.f. Depends: none. Ownership: fd.f. Claim: agent=worker workspace=.jj-ws.noindex/carl-fdsync.
