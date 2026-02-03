---
title: [LOW] Stack maps + safepoint design
status: open
priority: 3
issue-type: task
created-at: "2026-02-03T12:45:25.105164+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
---

docs/cranelift-parity.md:8 + src/jit/: no stack maps/safepoints. Fix: write design doc for Habu stack maps: encode live Value slots for VM + JIT frames; define safepoint protocol and root enumeration API; outline minimal implementation steps + tests. Verification: docs + follow-up dots.
