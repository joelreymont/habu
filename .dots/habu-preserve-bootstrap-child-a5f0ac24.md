---
title: Preserve bootstrap child diagnostics
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T08:46:50.043476+02:00"
---

Full context: test/nf.fs:7-12 stores only stdout, executes /tmp/nf-bin with stderr redirected to /dev/null, and discards child status. test/bootstrap-wide-memory.fs:10-14 therefore converts any cold-prefix rejection into the generic bootstrap wide memory mismatch. Reproducer: introduce an unavailable cold-prefix token such as 0<> in src/core/type-family.f, then run HABU_TARGET=macos-aarch64 gforth test/bootstrap-wide-memory.fs; the wrapper exits 1 with the memory mismatch, while /tmp/nf-bin exits 70 and reports the exact unavailable token. Fix: replace NFOUT-only capture with a structured child outcome preserving exit status, stdout, and stderr; make bootstrap-wide-memory assert rc 0 before comparing stdout and emit the captured diagnostic on failure. Keep the recovery path Gforth-only and add focused rc70/diagnostic and successful-output regressions. Files: test/nf.fs, test/bootstrap-wide-memory.fs, focused recovery fixtures.
