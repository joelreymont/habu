---
title: Publish right-transposed matmul
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T19:27:40.367836+02:00"
---

Why: GPT-2 forward needs C[i,j] = sum_k A[i,k]*B[j,k] (attention scores and the tied-embedding LM head) as a public compute primitive; MM-NT exists but is private to maki/attention.f. Exact result: move MM-NT ( ptr a ptr a ptr a n n n -- ) and private MM-NT-EL from maki/attention.f:14-24 into maki/matmul.f byte-identical, MM-NT public in package MAKI, attention.f consumes the public word. Zero behavior change. Loader-independent: no GPT2LOAD, WSTORE, or model-config contact. Owner: package MAKI in maki/matmul.f. Acceptance: attention suite green UNCHANGED; new maki/matmul-test.f cases proving MM-NT against MATMUL over an explicitly transposed operand on asymmetric non-square shapes (2x3 against the transpose of a 4x3), plus exact hand-pinned f64 values on integer inputs; both diff lints. Forbidden: behavior change, signature change, duplicate implementation left in attention.f.

Claim: agent=claude workspace=.jj-ws/habu-publish-right-transposed-8b484862
