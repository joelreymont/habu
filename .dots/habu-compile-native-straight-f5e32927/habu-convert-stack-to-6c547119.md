---
title: Convert stack to SSA
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:03.025328+02:00\""
blocks:
  - habu-elaborate-straight-line-72b55798
---

Full context: design section 7.3 makes stack SSA the native optimization center. Convert straight-line HIR stack effects to explicit typed values and block arguments; DUP/DROP/SWAP/OVER only rename value vectors. Acceptance: underflow/type/arity/source-binding negatives reject and pure stack renames create no executable operations. Dependency: straight-line HIR elaboration.

Claim: agent=stackssa workspace=.jj-ws/habu-convert-stack-to-6c547119
