---
title: Make fixpoint certify rejections blocking once clean
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:28:23.831525+02:00"
---

BF-CERTIFY-GENERATED (tools/build-fixpoint.f:863-865) treats VERIFY:SOURCE-BUF rejection of emitted stage sources as non-blocking report — a fail-open gate: a genuine type error in engine source (see the 0 USIGS ! dot) only warns and install proceeds. After all certify rejections are fixed and stage2-src/stdin-src certify rc 0, flip certify to blocking (E-BUILD-STATUS on rejection) so the self-host bootstrap window is fail-closed; keep an explicit documented escape only if bootstrap-recovery needs it (docs/bootstrap.md). Depends on: the 0 USIGS ! fix dot and the BF-CERTIFY-STDIN path dot.
