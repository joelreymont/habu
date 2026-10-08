---
title: Load Habu on Gforth through stages 1 and 2
status: open
priority: 1
issue-type: task
created-at: "2026-10-08T23:10:26.495988+02:00"
---

Problem: docs/bootstrap.md stages 1-2 rule that Gforth hosts Habu: a body per primitive row, the two spaces, a reader resolving only Habu words, and a codegen that compiles each definition from its checked events; then the Habu interpreter reads the rest. A prototype outside the repository proved it (62 boot-stream files, 1,366 checked definitions, 32 programs matching native) but reads seven facts from checker internals. Design: Plan report of 2026-10-08, trimmed by a complexity pass; the layer lives in src/host/gforth/ (Gforth is a host, docs/portability.md src/host/).
Acceptance: every child closed; docs/bootstrap.md stage 2 names `bin/hb --load test/gforth/host-test.f` and its counts instead of the prototype.
Out of scope: stages 3-4 (the build on Gforth, cross-build, self-check); deleting the seed bootstrap/cg, test/nf.fs, the bootstrap-* fixtures, tools/bootstrap.sh's seed path and src/habu/stage2.f, which waits until stage 3 writes hb from Gforth, since the from-zero path must not lapse; the editor check mode; tier 1 reading events. The Gforth host check stays a separate Gforth check, not part of test/run.f (CLAUDE.md, docs/gate.md).
Depends: children. Ownership: lead (carl). Claim: unassigned.
