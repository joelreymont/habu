---
title: Emit boolean masks directly
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T17:56:03.502454+02:00\\\"\""
closed-at: "2026-09-28T18:55:36.146697+02:00"
close-reason: Implemented direct canonical CSETM flags at qualified e319fdfe, preserving all conditions and exact false/true including NaNs and signed zero. All14 independent encoder vectors match; invalid conditions/registers refuse. Independent Astra review passed; B2-B5 and names converge;492/492 suites pass; strict signatures and both Maki stdin boards pass. Net engine code saving9036B, payload8900B, signed file0B due padding; engine remains2625655B SHA830c33d0af20d4202de584b62a94d63953d2b2f1f1af19830b0296a606ff2821. Maki signed file saves32832B, not added to engine goal. Lead verified gate summary, artifact hashes, board hashes and review. Integration differs from qualified source only in dots. Receipts ~/.cache/tmp/habu-native-mask-completion-20260928-01.md and habu-native-mask-review-20260928-01.md. No blocks edges remain.
---

Repeated-code source audit found native PUT-FLAG, PUT-FLAGI, PUT-FFLAG and PUT-FFLAGZ emit compare; CSET; NEG for canonical 0/-1. ARM64 CSETM directly replaces the final two instructions without a selector optimization pass. Implement minimal checked encoder and exact measure/write change, preserving condition and FP unordered semantics, source maps and register write accounting. This differs from rejected boolean-normalization folding whose optimizer cost erased savings. Quantify exact repeated pattern and B1/B2 full product economics including helper/metadata costs; retain only positive measured outcome. Use existing real-load canonical integer/FP flag tests, add any genuine missing behavioral witness before code; independent assembler and Astra review, native convergence/full registry/Maki/signatures before landing. No ABI, IR schema or image-format changes.
