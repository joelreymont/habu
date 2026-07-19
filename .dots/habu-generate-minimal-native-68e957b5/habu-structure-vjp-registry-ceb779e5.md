---
title: Structure VJP registry
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:33:00.666005+02:00"
---

src/arch/ptx/vjp.f:18-86 stores each vector-Jacobian product entry in five parallel columns: fixed name bytes+length, fixed expansion bytes+length, and saves n. Registration writes them serially, VJP-FIND returns -1, and length/save/id slips type-check, allowing the wrong expansion slice or tape-save count to enter generated backward PTX. Fixed per-row buffers reserve about 5 KiB before headers even when text is short. Define a nominal entry-id and typed save-count, STRUCTURE vjp-entry with name/expansion spans and save count, one bounded text arena, and a LAYOUT-BUFFER row store. Return option<entry-id> from lookup; preflight duplicate/table/text capacity and commit both spans+row atomically. Generated accessors replace column arithmetic. Preserve declaration grammar, registration/order/duplicates, expansions, save counts, reverse output, errors, gradients, and exact emitted PTX. Add checker negatives for id/length/save/span swaps, malformed/oversize declarations, injected allocation/arena failure rollback, duplicate/full table, lookup none/some, canaries, and every current expansion/gradcheck golden. Measure fixed versus used DATA, source/JIT/CODELEN, registration/lookup and AD generation throughput. Files: src/arch/ptx/vjp.f and focused VJP/AD tests. Verify VJP/autodiff/finite-difference/device/PTX/Maki/full native gates, typed-local diff, type/package/host/filemap/dot lints. Serialize with habu-pkg-ptx-vjp-1a41c708; ownership here is registry representation.
