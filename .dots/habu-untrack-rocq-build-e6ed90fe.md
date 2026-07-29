---
title: Untrack Rocq build outputs under formal
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T10:20:02.305723+02:00"
---

Full context: formal/Common/*.vo, formal/Common/.*.aux and .lia.cache are TRACKED in the repository, so every run of make -C formal or of either parity gate dirties the workspace with regenerable binary churn a worker must remember to restore before committing. Two lanes have already hit it and one nearly committed the churn. This contradicts the standing rule against committing large regenerable artifacts, and formal/Makefile's own header already says the generated makefile is a build output that is never committed. Untrack them, extend the ignore rules to cover formal/**/*.vo, *.vok, *.vos, *.glob, .*.aux and .lia.cache, and confirm both parity gates (test/compiler/ir-id-proof.f and ir-intern-proof.f) still build the model from a clean tree. Acceptance: after a fresh clone-equivalent checkout, make -C formal succeeds and jj st stays clean across a full gate run.

Claim: agent=cruft workspace=.jj-ws/habu-untrack-rocq-build-e6ed90fe
