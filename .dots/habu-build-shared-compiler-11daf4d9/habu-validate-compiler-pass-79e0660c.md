---
title: Validate compiler pass results
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.459853+02:00\""
blocks:
  - habu-encode-compiler-ir-545ee6d1
---

Full context: design section 6.7 requires pass-result ownership and witness headers bound to pass/version, input/output, target, numeric policy, schema, payload, and metrics. Implement PASS-VALIDATE in an independent package, validated-pass-result typestate, PASS-ACCEPT, PASS-RELEASE, and corrupt-binding fixtures. Acceptance: any header/payload mismatch rejects before payload interpretation or publication; every outcome preserves or consumes ownership exactly once. Dependency: canonical codec.

Claim: agent=irpass workspace=.jj-ws/habu-validate-compiler-pass-79e0660c
