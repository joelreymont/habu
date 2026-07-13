---
title: "Type DSL: prove hard-cutover fixpoint"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:47:06.947766+02:00"
blocks:
  - habu-type-dsl-enforce-19a93c1a
---

Rebase the integrated hard-cutover tree, regenerate the exact native fixpoint, and prove no legacy definer survives in source, generated stage/stdin/snapshot source, dictionary lookup, AOT images, or docs-facing grammar. Run typed-local diff lint, exact changed-file loads, test/run.f, maki/test.f, ptx-stdlib plus touched native gates, bootstrap/codegen parity, snapshot/AOT/owner-WID gates, host-lint, filemap-lint, trust/signature/namespace/reserved-name lints, and full gate timing. Move master only by verified-green fast-forward and push.
