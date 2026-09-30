---
title: Fold native constant shifts
status: closed
priority: 2
issue-type: task
created-at: "2026-09-28T13:16:47.889890+02:00"
closed-at: "2026-09-30T17:30:00.000000+02:00"
close-reason: "Dropped: the measured candidate 0fe1d896 saved 16 code bytes and added 1,224, 1,208 bytes more AOT code."
---

## Measured attempt rejected

Candidate `0fe1d896d6f8e9cce099487e3a58517347a68f5f` folds two earlier,
single-use scalar constants. Its source review and focused native semantic
checks pass, but B2 saves only 16 physical code bytes: 8 in PACKED-NARROW and
8 in SEQ-PUT. The new optimizer adds 1,224 bytes, yielding **1,208 bytes more
AOT code** (1,543,744 to 1,544,952). The signed file stays 2,790,775 bytes only
because padding absorbs the increase; DATA values also grow 208 bytes.

This implementation was rejected and is not in master. Full qualification was
stopped after B2 economics; no full gate, later convergence or Etch result is
claimed. Do not mark the optimization complete or substitute manual source
literals to hide the compiler gap. A further design needs a demonstrated net
benefit that includes optimizer code, or a separately justified broader pass.
Receipt: `~/.cache/tmp/habu-native-shift-completion-20260928-02.md`.
Exact rejected patch, binaries, disassembly and measurements are preserved at
`~/.cache/tmp/habu-native-shift-qualification-20260928-01/`.

## Original finding

Current B5 SHA24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b at sourcee337d11f emits PACKED-NARROW expression1 32 lshift as MOV1,MOV32,LSLV at VM0x100085858. Both operands are scalar constants; one shifted MOVZ suffices after native scalar correctionbf8e2023. Existing HIR shift lowering and native fold planner do not evaluate this expression. Fold two-scalar shifts in existing native selection/value machinery with target-cell shift count semantics; prove counts0,63,64,65,negative and shared operand behavior before code, preserve address kinds and remove producers only when uses permit. No general optimizer framework or cross-function propagation required. Audit receipt ~/.cache/tmp/habu-generated-code-audit-20260928-01.md. Measure actual native instruction/code/file delta, native convergence, focused semantic suites and full gate. Historical JIT shift fixf1a7f701 is separate. Lead owns integration/closure; Sol implementation after coupled scalar change.
