---
title: Generate minimal native code by construction
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:51:13.712172+02:00"
---

Task anchor: prevent redundant ARM64 emission by making helper effects and protection state explicit, lowering through one typed machine IR, canonicalizing before encoding, and rejecting unexplained region growth. Existing owners: habu-define-typed-arm64-4ab8894f owns helper ABI schema and schema-derived effect queries; habu-verify-emitted-arm64-efd5eb61 owns emitted-CFG liveness/frame verification. New children own protection-state typing, machine IR, canonical reductions, and per-region budgets. Acceptance: direct emitters consume the shared contracts, redundant saves/loads/transitions/repeated sequences are absent before encoding, clobber checks derive from the ABI schema, and region growth fails in the originating change with exact evidence. Files: src/arch/arm64/icode.f, src/habu/habu1.f, src/habu/habu2.f, tools/lint/clobber-lint.f, src/habu/engine-size.f, test/gate-build-size.f.
