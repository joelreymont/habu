---
title: Make maxima-rtest fail closed
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.490799+02:00"
blocks:
  - habu-canonicalize-maxima-root-32d71ea3
---

Problem: PLAN.md 2.4 still needs a worker-ready leaf covering process-level nonzero exit, authoritative upstream load identity, timeout classification, package-local setup flows, and strict runner/tool contract. Acceptance: tools/maxima-rtest.lisp exits nonzero on real failure, distinguishes timeout vs failure, uses authoritative loader identity, and supports documented package-local run_testsuite/test-batch flows only. Files: PLAN.md:744-769, tools/maxima-rtest.lisp, src/main.zig, src/interp/repl.zig. Verify: success, failure, invalid-root, and timeout probes all produce correct exit status and authoritative provenance.
