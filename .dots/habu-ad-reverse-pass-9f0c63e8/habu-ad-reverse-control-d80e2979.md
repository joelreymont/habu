---
title: AD reverse control-flow reject
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T08:55:36.559382+02:00"
closed-at: "2026-06-30T09:01:08.988359+02:00"
close-reason: "completed locally: AD reverse rejects control-flow tokens with E-PTX-AD-CONTROL; focused PTX suite, stdlib error fixture, lints, cold and hot local full suite green; zed untouched"
---

Problem: AD reverse v0 is straight-line only, but control-flow tokens currently fail only as generic missing VJP/unknown words. Fix: make lib/ptx/ad.f reject IF/ELSE/THEN/BEGIN/WHILE/REPEAT/UNTIL/DO/LOOP/+LOOP/LEAVE/RECURSE/CASE/OF/ENDOF/ENDCASE with a named PTX AD control-flow error before VJP expansion. Verify: focused ad-test negative fixture catches the named error; ptx-stdlib focused suite passes; typed-local/filemap/host/dot/stale lints pass; full local hot suite green. Zed/device validation out of scope.

2026-06-30 local proof: added `E-PTX-AD-CONTROL`, case-insensitive `AD-CONTROL?`, and `AD-REQUIRE-STRAIGHT`; both `VJP-ADJOINT` and `VJP-EXPAND` now reject control-flow tokens before adjoint lookup. `lib/ptx/ad-test.f` proves lowercase `LOAD if STORE then` throws the named error. Focused PTX static suite passed; `stdlib-errors-test` passed; typed-local-diff-lint, dot-dep-lint, stale-status-lint, host-lint, and filemap-lint passed; full local native suite passed cold 44087ms internal / 46.293s wall and hot 24622ms internal / 26.843s wall. Zed/device validation intentionally untouched.
