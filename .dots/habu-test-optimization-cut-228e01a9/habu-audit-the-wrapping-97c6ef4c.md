---
title: Audit the wrapping length guards in src/
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T17:34:37.515143+02:00\""
---

Problem: the lib audit (change kzxssvzv) found 65 lines under src/ of the same shape, a sum of a length or offset compared with a capacity, and did not assess them; the list is the census to redo, not evidence. The ones whose added value looks caller-supplied: src/core/include.f:210, :234, :692; src/os/image-bytes.f:106; src/core/roles.f:27; src/core/render.f:58; src/core/internal-mark.f:103; src/core/type-family.f:184; src/habu/verify-source.f:210; src/habu/aot-capture.f:335, :631, :2295; src/habu/prims.f:140; src/habu/aot-file.f:397, :477; src/habu/aot-lib.f:138, :417; src/compiler/native/elaborate.f:3771; src/compiler/native/trap.f:70; src/compiler/ir/attr.f:1245; src/compiler/ir/type.f:821; the `need +` grow checks in src/core/checker.f:743, :1859, :3896, :4032, :5505, :7555. src/os/env-base.f TMP-PATH belongs to habu-hold-every-path-09bc6119. Acceptance: every such guard under src/ is listed with whether the added value can be supplied by a source file, an argument, the environment or a file the engine reads; each reachable one refuses negative and wrapping values with its existing error by comparing against the room left; boundary cases are written first through the real load path; the engine is rebuilt and the generation chain converges. Files: src/**/*.f and the owning tests. Verify: the owning suites, tools/two-generation-build.f, the full native suite at integration. Depends: habu-hold-every-path-09bc6119 (same files under src/os and src/core). Ownership: guard expressions in src/. Claim: agent=kestrel workspace=.jj-ws/r4-wrap-src.
