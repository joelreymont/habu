---
title: Pin immutable native engine ceilings
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T23:24:17.983753+02:00"
---

Add independent immutable payload ceilings alongside exact per-platform no-growth baselines. Contract: __text <=110592 on every target; macOS file <=132343; Linux file <=114880 after exact Orin verification; native type integration <=8192 with compile/adt and compile/p2wide each <=3072. Prove the emitted region-map sum equals the candidate payload and exercise baseline-growth, stale-baseline, payload-cap, and platform-cap failures independently. Files: test/gate-build-size.f, test/gate-engine-lib.f, src/habu/engine-size.f, docs/size-rca.md. Acceptance: focused size gate, fixpoint x2, macOS exact artifact proof, Orin exact artifact proof; no baseline bump can bypass the ceiling.
