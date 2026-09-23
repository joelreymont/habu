---
title: Build refuses a trusted family value the JIT admits
status: open
priority: 3
issue-type: task
created-at: "2026-09-24T00:43:25.759932+03:00"
---

Problem: TRUSTED: FORGE ( -- gwfn ) 0 5 ; over ENUM gwfn 0 VARIANT gnn ;VARIANT VARIANT gns FIELD value n ;VARIANT ;ENUM loads and runs under bin/hb --load (rc 0, engine 8947cf0f), but tools/hb-build.f on the same source dies rc 67 'ncomp: cannot compile FORGE' with -8503 E-NELAB-JOIN, while the control TRUSTED: PAIR ( -- n n ) 0 5 ; builds and runs. A trusted word answering a family value from literals is therefore unbuildable: test/runtime-regression-test.f GWF-FORGE and GWN-FORGE are that shape, and an FFI boundary answering a sumtype would be too. Probes under ~/.cache/tmp/hazel-probe: trusted-enum-subject.f (refused), trusted-pair-subject.f (control), forge-drop-subject.f (the same refusal). Related: b343e371 (KIAPI:FOOTPRINT-POSE -8503). Acceptance: name the layer (the width the native elaborator gives a trusted family result against the tier that ran it under the JIT), fix it, and a fixture builds the trusted-enum shape stripped and runs it, the forged value then reaching the fetch guard (rc 85, hb: bad layout tag). Files: src/compiler/native/elaborate.f, tools/hb-build-lib.f. Verify: test/gate-aot-positive.f, test/runtime-regression-test.f, test/run.f. Ownership: hazel. Claim: unassigned.
