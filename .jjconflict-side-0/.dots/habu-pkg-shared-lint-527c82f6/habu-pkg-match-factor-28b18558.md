---
title: Package match-factor bench source
status: closed
priority: 1
issue-type: task
created-at: "2026-07-23T06:20:47.334950+02:00"
close-reason: "Landed as 37108aa73d38."
---

Problem: test/match-factor-pin.f:300 emits a global SRC buffer into a SUBJECT
child that inherits the resident runner dictionary. test/gate-runner-support.f
already loads test/prop-test-core.f, whose global SRC makes the real
tail-process runner reject the benchmark with duplicate publication rc 78.

Required result: the generated benchmark program opens package MFP-BENCH
before declaring bres, SCAP, SRC, SU, T0, EVL, C+, S+, N+, GEN, BUILD, and
RUN-BENCH. It executes RUN-BENCH while that package is open, closes the package
before the final ok output, and changes no benchmark workload, generated
definitions, timing rule, legacy MATCH pin, or parent harness behavior. Do not
rename SRC as a substitute for ownership, add an alias or exception, copy the
runner, or publish a forwarding global.

Package owner: MFP-BENCH owns every generated benchmark definition.
MATCH-FACTOR-PIN continues to own only the parent fixture.

Acceptance: removing the package scope reproduces rc 78 through the real
tail-process runner. The fixed production command passes twice. The standalone
match-factor fixture remains green. The benchmark still compiles exactly 150
MATCH and 150 construct words and prints its timing plus ok. Exact package and
typed-local diff gates pass.

Files: test/match-factor-pin.f only.
Smallest real check: bin/hb --load test/gate-runner-support.f
test/gate-runner-entry.f -- tail-process --pool-slots 8.
Depends: none.
Owned result: generated match-factor benchmark namespace only.
