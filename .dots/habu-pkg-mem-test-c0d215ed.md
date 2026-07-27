---
title: Package memory-test fixtures
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T23:22:48.841607+02:00"
---

Why: `lib/memory-test.f` defines four `MEMT-*` boundary fixtures and
`STAT-MEM` outside its existing package. Any body edit therefore fails package
ownership.

Owner: the existing `MEM` package in `lib/memory-test.f`.

Behavior: move the five file-local white-box helpers into that package and use
short private tails. Preserve load order and behavior. If any external caller
exists, stop and redesign; do not export a bridge.

Acceptance: a representative body edit to every moved word passes the package
diff lint; `bin/hb --load lib/memory-test.f`, both diff lints, and the owning
standard-library slice pass.

Reviewed implementation evidence:
`d72514eb7d32156171b8b7ee94bb922049f32c1e` satisfies this owned result. The dot
remains active until the reviewed change is integrated and verified.

Claim: agent=vecmem workspace=.jj-ws/habu-pkg-vecmem
