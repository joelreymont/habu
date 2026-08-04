---
title: Bound PTX host marshalling buffers
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T23:52:35.786747+02:00"
blocks:
  - habu-add-checked-mem-ebd95492
---

Problem: PTX host marshalling still exposes SF-ST, SF-LD, F32-PACK, F32-UNPACK, SF-ST16, F16-PACK, and BF16-PACK over raw pointers and unchecked element counts. Moving those words into a new shared package would preserve the same unbounded authority rather than fix it. After the common checked MEM subspan and alignment capability lands, replace these APIs with bounded typed byte and cell spans whose extents prove every four-byte or two-byte access, whose count multiplication cannot overflow, and whose read/write roles and lexical lifetimes are explicit. Keep scalar IEEE-754 F64/F32/F16/BF16 conversion in its state-free numeric owner; this leaf owns only host-buffer addressing and marshalling. Migrate every PTX, Maki, ONNX, CUDA tool, and device-test consumer without compatibility wrappers. Acceptance: short destination/source spans, negative or overflowing counts, misaligned typed paths where alignment is required, cross-region spans, post-scope use, and element-count times width overflow reject before access; byte-aligned wire operations preserve exact little-endian bytes and canaries; F32/F16/BF16 device goldens remain exact; no public raw marshalling pointer API remains. Verify common memory suites, PTX standard library and toolchain, ONNX and Maki device slices, typed-local/trust/package/host/dot lints, and the full native gate.
