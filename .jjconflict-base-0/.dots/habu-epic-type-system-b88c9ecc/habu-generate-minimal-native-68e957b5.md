---
title: Generate minimal native code by construction
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:51:13.712172+02:00"
---

Task anchor: prevent redundant ARM64 emission by making helper effects and protection state explicit, lowering through one typed machine IR, canonicalizing before encoding, and rejecting unexplained region growth. Existing owners: habu-define-typed-arm64-4ab8894f owns helper ABI schema and schema-derived effect queries; habu-verify-emitted-arm64-efd5eb61 owns emitted-CFG liveness/frame verification. New children own protection-state typing, machine IR, canonical reductions, and per-region budgets. Acceptance: direct emitters consume the shared contracts, redundant saves/loads/transitions/repeated sequences are absent before encoding, clobber checks derive from the ABI schema, and region growth fails in the originating change with exact evidence. Files: src/arch/arm64/icode.f, src/habu/habu1.f, src/habu/habu2.f, tools/lint/clobber-lint.f, src/habu/engine-size.f, test/gate-build-size.f.

2026-07-19 audit charter: this is also the requested optimization parent for evidence-backed codebase cleanup. Its new children cover measured native/PTX/source/DATA bloat, dead or dormant code adjudication, package-prefix pseudo-namespaces, missing STRUCTURE/payload-ENUM modeling, duplicated architecture, unsafe raw representations, and ordinary correctness defects that make optimization claims false. Each child owns one proved seam, records before/after size or runtime evidence where applicable, preserves intentional REPL/profiler/xref/debugger product features, and prefers deletion over polishing when measurements falsify an unreachable experiment.

Capability boundary proved on current master: unified STRUCTURE and payload-bearing ENUM are specified by habu-epic-one-structure-04f9804f but not operational; current docs and compiler still expose PRODUCT, SUMTYPE, and payloadless ENUM, while STRUCTURE is E-UNDEFINED. Every child that names the unified surface is a post-cutover target and must wait for the exact green proof habu-type-dsl-prove-93da83c4, or have its acceptance criteria explicitly absorbed by the matching library/tool/Maki migration owner. Do not implement a new legacy PRODUCT/SUMTYPE version or assume the cutover already landed.
