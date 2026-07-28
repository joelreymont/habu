---
title: Add in-place buffer GELU
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T19:27:40.379049+02:00"
---

Why: the GPT-2 MLP forward applies GELU element-wise over an activation buffer; only scalar GELU-F ( r -- r ) exists (maki/gelu.f:19). Exact result: public MAKI:GELU! ( ptr a n -- ) in maki/gelu.f applying the existing scalar GELU-F in place over n cells via T-GET/T-SET. The scalar word is the committed oracle; no second GELU formula appears anywhere. Loader-independent: no GPT2LOAD, WSTORE, or model-config contact. Owner: package MAKI in maki/gelu.f. Acceptance: exact per-element parity with GELU-F over a mixed-sign buffer; an in-place proof (source buffer identity, values replaced); n=0 no-op; GELU-F(0)=0 exact; gelu and mlp suites green unchanged; both diff lints. Forbidden: a second GELU implementation or approximation constant, allocation, out-of-place variant, loader types.
