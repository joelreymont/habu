---
title: Model GPU reduction domains
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:41.395537+02:00"
blocks:
  - habu-validate-elementwise-gpu-20ac3ed4
---

Full context: design GPU Wave C adds row domains, inactive-lane masks, reduction identities, and explicit numeric policy to GPU-KIR. Acceptance: domain coverage, empty/masked rows, identity/type/policy, and source-binding mutations reject; canonical row-reduction fixtures pass.
