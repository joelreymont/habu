---
title: Migrate model IR mark
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:28:39.971298+02:00\""
---

Why: maki/model-ir.f is shared by model capture, optimization, backward construction, and native inference but still declares MIR:mark with legacy PRODUCT. Owner: maki/model-ir.f and maki/model-ir-test.f only. Replace PRODUCT mark directly with STRUCTURE in the existing public MIR package, preserving nodes/slots/refs schemas and order, MIR-MARK:MAKE/UNMAKE spelling, three-cell layout, MIR-MARK/MIR-RELEASE rollback semantics, table high-water invariants, errors, allocation ownership, and every public interface. Update product comments. Forbidden: aliases, legacy parser edits, raw casts, mark/rollback redesign, table changes, caller migrations, or unrelated cleanup. Pre-change proof: token-aware production census finds exactly one executable PRODUCT declaration in the file. Acceptance: the real model-ir suite exercises generated MAKE/UNMAKE through MIR-MARK and MIR-RELEASE, rejection of growing or invalid marks, rollback after graph growth, and unchanged model rendering; exact reflection/effects/layout remain stable; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-model-ir-structure workspace=.jj-ws/habu-migrate-model-ir-36e5e9b0
