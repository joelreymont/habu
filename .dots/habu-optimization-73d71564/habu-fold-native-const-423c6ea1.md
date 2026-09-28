---
title: Fold native constant shifts
status: open
priority: 2
issue-type: task
created-at: "2026-09-28T13:16:47.889890+02:00"
---

Current B5 SHA24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b at sourcee337d11f emits PACKED-NARROW expression1 32 lshift as MOV1,MOV32,LSLV at VM0x100085858. Both operands are scalar constants; one shifted MOVZ suffices after native scalar correctionbf8e2023. Existing HIR shift lowering and native fold planner do not evaluate this expression. Fold two-scalar shifts in existing native selection/value machinery with target-cell shift count semantics; prove counts0,63,64,65,negative and shared operand behavior before code, preserve address kinds and remove producers only when uses permit. No general optimizer framework or cross-function propagation required. Audit receipt ~/.cache/tmp/habu-generated-code-audit-20260928-01.md. Measure actual native instruction/code/file delta, native convergence, focused semantic suites and full gate. Historical JIT shift fixf1a7f701 is separate. Lead owns integration/closure; Sol implementation after coupled scalar change.
