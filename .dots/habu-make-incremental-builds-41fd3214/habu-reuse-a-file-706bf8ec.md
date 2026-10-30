---
title: Reuse a file whose run cuts or rolls back definitions
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T21:04:26.418882+02:00"
---

test/native-file-auto-e2e.f CUT-CASE fails on the lane 11 tip (mlluspps) and on H (qqwrsvrm): asserts 8/9 of `-- cut` (250/251 in the full run). app.f defines CUT-OLD, cuts it with HIDE-DEFS-FROM, then defines CUT-NEW and MAIN. The cold object drops CUT-OLD's row (records=2). On the warm build the live `: CUT-OLD` head meets CUT-NEW's row, misses, F-MISS counts it and SPIKE-RUN refuses app.f reason=head-stale, so the file never reuses and stores its object on every warm build. With next-row matching (lane OWN) a head the cut retired cannot be told from a changed head unless the object says so; vqqlxukr solved the same shape for undefine by keeping the retired record's row (the warm load installs it and the file's undefine retires it again). Evidence: carl-dots/.out/carl-file/l11-foldins.md 'test/native-file-auto-e2e.f on the tip'. Rolled-back definitions take the same path (auto e2e GENERATED-CASE 214-216, 223; carl-fauto/.out/triage/auto-triage.md section G): the kernel roll back of a caught evaluate cuts SDFAIL:MAKE/UNMAKE through FILE-CUT, the cold object has no row for them, their live evaluated heads miss and decl.f is refused head-stale on every warm build. One fix for both: FILE-CUT keeps the cut rows as vqqlxukr does for undefine. Lane CUT: workspace .jj-ws.noindex/carl-fcut on zxztprpo, brief carl-dots/.out/carl-file/brief-fix-cut.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fcut.
