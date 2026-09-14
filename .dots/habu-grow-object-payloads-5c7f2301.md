---
title: Grow object codec cache and linker payload storage
status: closed
priority: 1
issue-type: bug
created-at: "2026-09-14T12:30:00Z"
close-reason: "Checked dynamic payload storage and optimized large roundtrips pass; parent owns combined stripped-build integration."
---

At integration base 180b7e63, OBJ capped encoded records at 256 KiB and each raw hex section at 128 KiB; OBJSTORE preallocated that same fixed maximum, and OBJLINK separately capped each merged text/data section at 64 KiB. A correctly produced native C5 quotation object was 525,288 bytes (SHA-256 b0ee2d0a65f55e1b5f90eb2b8870c4d1e161cd89da1c61277129a2e7f92bba64), so hb-build threw E-OBJ-CAPACITY (-3701) after its native child succeeded.

Use registered DYNAMIC-BUFFER storage for codec, cache reads and linked sections. Check cell rounding, hex expansion, append sums and relocation ranges before allocation or access. MAX-BYTES now means the representable cell-backed extent; its only preallocation consumer was replaced by file-sized reads. Preserve the schema, metadata caps, READ-ALL truncation checks and digest validation. LOAD validates without modifying its input, and append inputs are rebased when they alias a growing codec buffer.

Optimized checks use /tmp/cedar-numeric-native (SHA-256 2a49a29c9804f00292652d45f0a32aa27e6f224034585501e890bf9d2acbc14c) with an explicit 1 set-tier prelude. Codec, cache, resolver, linker and image suites pass. Resolver coverage includes 528,385-byte text and data, alias self-load, content/index resolution, preserved bytes across linker growth and an aliased append that forces codec relocation. Small metadata refusals remain covered; the former 64 KiB merge refusal is now a passing 17-object append.

The real C5 raw artifact also passes codec→cache→load→link→OBJIMG. The rebuilt 590,016-byte image has SHA-256 34da922e65d017397a3bd27a904aff5de6680b5c1caaba1c87efa23cdadecbd2, identical to the direct-link image, and fresh execution exits 0. OBJIMG owns no payload buffer; the assembler's separate existing 32 MiB image budget remains outside this library fix. Full combined native build and gate remain the parent integration lane's responsibility.
