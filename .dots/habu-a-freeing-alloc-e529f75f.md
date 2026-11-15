---
title: A freeing allocator for blocks of any size in wasm
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:32:36.140922+01:00"
---

Problem: Maki's view (wasm, viewer.f) must hold each range it downloads - node pages of a few KB, mesh heads under 1 KB, mesh blocks up to 1 MiB - in storage of its own and free it when the range is evicted, so that wasm memory stays within a budget as a person pans and zooms a large model (Maki goals: loads only what it draws, holds no multi-megabyte object where that can be avoided). Today viewer.f HOLD-ALLOC takes one MEM-ALLOC-BYTES allocation per 1 MiB answer and frees it only when every range in it is freed (UNSHARE), so an evicted block's bytes stay while any neighbour lives. HEAP is libc's and native only; ARENA frees nothing per block. Wanted: an ALLOC record (src/lib/allocator.f) that runs on the wasm target, frees blocks of any size from a few bytes to 1 MiB, reuses freed space, and adds bounded overhead per block (no rounding of each block to 64 KiB). Acceptance: on the wasm target, allocating and freeing 100000 blocks of mixed sizes from 100 B to 1 MiB in random order keeps the mapped bytes within 1.25 times the live bytes plus a constant. Depends: none. Claim: unassigned.
