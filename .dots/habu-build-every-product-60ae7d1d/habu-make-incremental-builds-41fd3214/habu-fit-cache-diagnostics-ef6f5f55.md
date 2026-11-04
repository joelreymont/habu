---
title: Fit cache diagnostics in captured test output
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.564378+02:00"
closed-at: "2026-10-31T10:27:49.419918+02:00"
close-reason: "Landed 'Capture fit cache diagnostics whole': the build runs through CMD (src/lib/process-command.f), whose streams grow; IO-CAP buffers gone. With HABU_NATIVE_CACHE_DIAG=1: rc 0 'test: ok', 238 s, build.err 10,377,379 bytes (was uncaught -2504 at 2.6 s). Review fold-in: require process.f, comment. Seen under diag, not chased: the second (x86-64 target) build reused 0 / compiled 176 with 14,777 cache-store-refused lines."
---

Problem: test/prefix-provided-unfilled.f run with HABU_NATIVE_CACHE_DIAG=1 on the outer process dies uncaught -2504 (E-PROC-TRUNCATED): the child build's diag output overflows the test's IO-CAP $8000 capture. Acceptance: the diagnostics reach the reader without losing the test result (capture to the tree's build.out/err files rather than a fixed buffer, or the harness's file capture). Files: test/prefix-provided-unfilled.f, the PROC-CWD capture used. Verify: the test with diag set, rc 0. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fdiag (brief carl-dots/.out/carl-file/brief-fdiag.md).
