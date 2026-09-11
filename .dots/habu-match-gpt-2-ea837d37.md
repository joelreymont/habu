---
title: Match GPT-2 architecture directly
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.399901+02:00"
closed-at: "2026-08-02T16:44:32.276012+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
---

Why: GPT2LOAD's only MODEL semantic-enum use can match the exact MDLCFG architecture arm directly. Result: replace that check with exhaustive MDLCFG:arch matching and no semantic adapter. Owner: the one GPT2LOAD architecture check and focused fixture only. Production red: deleting MODEL enums would leave this caller. Acceptance: GPT-2 accepts, every non-GPT-2 arm rejects by name, and no MODEL symbol remains in GPT2LOAD. Forbidden: generic family, adapter, fallback arm, version, or compatibility mapping. Smallest owning check: the GPT2LOAD configuration test.
