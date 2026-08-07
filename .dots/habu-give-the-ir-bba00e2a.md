---
title: Give the IR a vector type kind and the a64 dialect a V class
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T20:14:57.140561+02:00"
blocks:
---

src/compiler/ir/type.f has eight kinds (type.f:120-129): int float pointer quotation code-ref memory-token mask opaque. No vector kind, and it is not among the design's deferred ones either (type.f:19-24 names tuple/layout, register-class, tensor/memref) - so this is a new design entry.

Shape is already set by the file. Interning is structural over a fixed four-cell row OFF-KIND/OFF-A/OFF-B/OFF-C via INTERN4 (type.f:184-188, 542), so K-VEC(element type ordinal, lane count) fits with no new substrate. The target-feature gate is FMT-CK (type.f:330-343), called from FLT (type.f:702-705) before INTERN4; copy it line for line as VEC-CK against CTARGET:F-SIMD. That feature already exists and is already legal on aarch64: src/compiler/target.f:106 BIT-SIMD, :261 F-SIMD, :119 the arch mask includes it. Also needed: wire code in KIND-CODE/N>KIND (type.f:205-275, both exact MATCHes that throw E-IR-TYPE-STATE on an unmatched code), a RENDER case, and acyclicity re-verification for the element reference.

Then the a64 dialect. src/compiler/native/a64ir.f:323-329 names this exact seam in its own words: 'A value of this dialect is a 64-bit general register, a 64-bit floating register, or the memory token... The SIMD register file, labels and fixups are further records of the same dialect and are not here yet; the seam where they arrive is these three readers.' So add VEC-TYPE beside GPR-TYPE/FPR-TYPE/MEM-TYPE (a64ir.f:707/714/724), and a SIMD-TARGET macro beside FP-TARGET (a64ir.f:564-566).


Found by agent neon while scoping habu-vectorize-the-byte-a0da35a7.

Unblocked 2026-08-07: the interference file-keying prerequisite landed and closed (e9aa49a4).
