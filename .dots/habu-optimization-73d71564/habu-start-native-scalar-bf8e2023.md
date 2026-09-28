---
title: Start native scalar MOVZ in a useful half
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-28T13:10:51.599041+02:00\\\"\""
closed-at: "2026-09-28T14:32:43.875379+02:00"
close-reason: Native scalar MOVZ starts at its first nonzero half and costs exactly that chain; MOVN policy and relocation carriers stay separate. Source36f93d2e passed independent review03, focused native semantics, B2–B5 and all sidecar equality, full491 gate and both actually executed Maki smokes with unchanged board hashes/signatures. Qualified SHA7a357464fe741b9359a170b433b54364a431df117f21ee947f2caca25d8044bb remains2807287B; AOT code1543368 to1543112 (-256B), Maki code-1232B; no whole-file reduction claimed. Receipt ~/.cache/tmp/habu-native-scalar-completion-20260928-01.md.
---

Current engine24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b at sourcee337d11f emits MOVZ zero then MOVK for scalar65536 in PACKED-NARROW, MEM-64K-BYTES, MEM-64K-COUNT-FOR and A64IR:IMM-LIMIT. Those four observed pairs need one shifted MOVZ each; 16 code bytes is only a local evidence floor, not net engine saving. Native select.f MOVZ-COST and MATERIALISE-Z force half0 although its independent MOVN path and the JIT assembler choose a useful half. Fix native scalar selection: first nonzero half (half0 for zero), other nonzero halves via MOVK, cost nonzero halves with minimum one, existing MOVN tie policy and typed address carriers unchanged. Establish real-load semantic coverage gaps before code; verify zero/allones, halves1/2/3, mixed and MOVN-favorable scalars, shared operands and relocation separation. Measure before/after disassembly and actual native engine sections, native byte convergence, full gate and Maki smokes. No blanket inline, shift-fold or division change in this dot. Lead owns integration/review/closure; Sol owns implementation and qualification. Evidence ~/.cache/tmp/habu-generated-code-audit-20260928-01.md.
