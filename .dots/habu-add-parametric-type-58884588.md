---
title: Add parametric type-mismatch negative regressions
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:08:38.724794+02:00"
---

M2 (parametric checker) is BUILT+LANDED in src/core/checker.f and works, but the only committed PTX tests (lib/ptx/header-test.f) cover %BLOCK negatives + positive KERNEL: checks - NOT parametric type-mismatch rejects. Pin the working rejects as committed regressions in the gate so a future checker refactor cannot silently lose them.
- Files: extend lib/ptx/header-test.f (or a new tools/ptx-check-test.f) with negative fixtures: span space-global vs space-shared rejects; extent-r vs extent-c rejects; tile mask-token mismatch rejects; malformed span< (missing >) rejects with SGBAD. Use TTHROWS / a checked reject-capture, assert the diagnostic substring.
- Verify: each negative is REJECTED (exit nonzero / named throw) through the owning bin/hb --load path; wired into test/run.f gate.
- Dep: none (capability exists). Was M2f, rescoped after discovering M2 is already built.
