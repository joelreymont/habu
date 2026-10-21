---
title: Share XML owner refusal setup
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:43:02.218105+03:00"
---

xml-owner-e2e has five refusal children each loading/checking XML anew; current fullgate sampled29.039CPU for the row. These refusals test linear storage/private owner authority, not cold module initialization or CLI startup. Load XML once in the parent and use existing SUBJECT:RUN fork isolation for just these five snippets; preserve exact70/diagnostic checks, private childstate, real source consumer, native build/execution and edited XML artifacts. Measure actual row before/after with source/native artifacts unchanged; independent review. No generic fork suite adapter or cached verdicts.
