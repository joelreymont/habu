---
title: Emit boolean masks directly
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T17:56:03.502454+02:00\""
---

Repeated-code source audit found native PUT-FLAG, PUT-FLAGI, PUT-FFLAG and PUT-FFLAGZ emit compare; CSET; NEG for canonical 0/-1. ARM64 CSETM directly replaces the final two instructions without a selector optimization pass. Implement minimal checked encoder and exact measure/write change, preserving condition and FP unordered semantics, source maps and register write accounting. This differs from rejected boolean-normalization folding whose optimizer cost erased savings. Quantify exact repeated pattern and B1/B2 full product economics including helper/metadata costs; retain only positive measured outcome. Use existing real-load canonical integer/FP flag tests, add any genuine missing behavioral witness before code; independent assembler and Astra review, native convergence/full registry/Maki/signatures before landing. No ABI, IR schema or image-format changes.
