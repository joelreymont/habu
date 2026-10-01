---
title: Throw or retire E-ZED-EMIT
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T12:03:31.896900+02:00"
---

Problem: lib/errors.f:238 defines -4007 E-ZED-EMIT ('local artifact emit (bin/hb spawn) failed') and nothing in the tree throws it (rg at r4-wblabel f2d76c1c; Etch is a separate repo). Either a zed-run path that spawns bin/hb to emit an artifact reports its failure under another code and should throw this one, or the constant is dead. Found by the r4-wblabel c3 worker. Acceptance: decide with evidence (rg of every bin/hb spawn in tools/zed-run-lib.f and its callers; Etch's references checked read-only); then either that path throws E-ZED-EMIT with a failing-first case through tools/zed-run-test.f, or the constant is replaced by the file's retired-number comment (as -4006 and -9102). tools/error-code-lint.f rc 0. Files: lib/errors.f, tools/zed-run-lib.f, tools/zed-run-test.f. lib/errors.f is baked: rebuild g1 and run tools/two-generation-build.f.
