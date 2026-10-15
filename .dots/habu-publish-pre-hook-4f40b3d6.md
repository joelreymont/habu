---
title: Publish pre-hook seed checker facts
status: active
priority: 0
issue-type: task
created-at: "\"2026-10-15T08:10:53.043760+03:00\""
---

Fresh Gforth recovery after reviewed occurrence-order fix c3999ed4 fails checked xref grid-retire-owner on CHECKER-OWNER-ABI:GRID-RETIRE-OFF even though scope-find sees its live dictionary constant. checker.f publishes sibling pre-hook constants with PPRIM but omits this one. Scratch addition of the exact numeric-output axiom passes that failure and reveals E-NONCALLABLE in callable, which needs separate precise localization within the same pre-hook publication boundary. Publish only proved missing effects for existing pre-hook declarations; no shadow constants, ABI downgrade, unchecked wrappers or checker weakening. Existing bootstrap-engine-stack.fs is the red-first behavioral check; continue documented check-only recovery to a current private host, review independently, then required native/recovery gates. The older native source-only refresh retains its old core ABI and cannot introduce this field through its certify-only prefix; use the corrected seed rather than masking that incompatibility.
