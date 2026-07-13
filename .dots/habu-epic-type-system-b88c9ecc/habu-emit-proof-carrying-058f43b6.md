---
title: Emit proof-carrying register allocation certificates
status: open
priority: 3
issue-type: task
created-at: "2026-07-13T11:44:22.408382+02:00"
blocks:
  - habu-differentially-test-arm64-7b6e4269
---

Context: Model CAD V2 requires a smaller trusted core and evidence-gated promotion. Even with CFG checking, consumers currently trust the compiler run that selected physical registers and spills. Fix: emit a compact certificate bound to the exact source, typed IR, target, CFG, and code hash containing live ranges, physical assignments, spills and frame slots, call clobbers, preservation facts, SP proof, and verifier version. Add an independent checked verifier that accepts the certificate before image or kernel promotion and emits structured evidence; no compiler-internal mutable state is trusted by the verifier. Acceptance: valid native and kernel allocations verify; mutations to code, CFG, assignment, spill slot, call effect, hash, or certificate reject with structured diagnostics; certificate verification is deterministic and content-keyed; promotion requires the evidence row.
