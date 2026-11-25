---
title: "Build a program whose FUNCTION: row takes a str"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:15:24.758515+01:00"
---

Problem: a build that loads src/lib/zip.f is rejected at src/lib/zip-ffi.f:23 `FUNCTION: C-PWRITE pwrite ( n str n -- n ) ;FUNCTION` with E-MISMATCH (`:}` takes n; the stack holds @str.tag), so test/file-objects.f cases Q and ZP fail on master; check.f --verify-only on zip-ffi.f passes. Reproducer: tools/hb-build.f on test/fixtures/file-objects/kept-main.f. Acceptance: the cause found and fixed; the build passes and file-objects is green. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-zipffi.
