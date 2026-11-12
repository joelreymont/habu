---
title: Test the seal in --repl and a denied unlink
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:27:56.610002+02:00"
---

Problem: the seal review found two checks missing. No test builds a --repl image whose program source reopens an engine package (tools/hb-build-core.f REPL-BODY seals before program source; test/file-objects.f SQUAT-CASE covers only the stripped build). The rewrite of src/lib/fs-rename-noreplace-test.f dropped the failed unlink after the no-replace link, which a real program reaches: a directory ACL 'everyone deny delete,file_inherit' makes FS:ATOMIC-CREATE-PRIVATE's temp-file unlink fail with EACCES after the destination is published. Acceptance: a --repl build of squat-main.f exits 84 naming CHECKER-RESOLVE at its line; FS:ATOMIC-CREATE-PRIVATE in such a directory throws E-FS-IO with EACCES and leaves the destination published, on macOS, and the test leaves no tree Finder cannot delete. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-tests.
