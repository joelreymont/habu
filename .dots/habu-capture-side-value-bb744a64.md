---
title: Capture-side value-range relocation scan
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T06:27:09.001195+02:00\""
---

Deferred from the direct-BL landing (1e9a3926) per separate-change discipline: ACAP-SCAN-DATA/ACAP-SCAN-CODE value-range scanning is a DISTINCT relocation class (address literals in data/code, not calls) and was left untouched - orthogonal to and unbroken by the call conversion. Own it: recognize/canonicalize address-literal relocations at capture, re-encode at boot, with the same by-name discipline as call sites. src/habu/aot-capture.f + habu2.f; CODELEN same-commit.

Claim: agent=capval workspace=.jj-ws/fable-capval machine=spark (owns the capture-side address-literal relocation scan: src/habu/aot-capture.f + habu2.f re-encode + CODELEN same-commit)
