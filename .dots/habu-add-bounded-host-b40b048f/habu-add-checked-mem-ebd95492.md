---
title: Expose cell-aligned MEM bases
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:21.424964+02:00"
---

Problem: BPE and MODEL-ASSET allocate one anonymous mapping containing
cell-width header fields followed by byte tails. `MEM:ALLOC-BYTES` exposes only
`ptr u8`, which forces each consumer to assert the same byte-to-cell pointer
reinterpretation. The alignment fact originates at the successful anonymous
`mmap` result: its base is page-aligned and therefore cell-aligned. It is not an
arbitrary byte pointer requiring a runtime refinement result.

Result: strengthen the existing audited syscall-result boundary to
`MEM-ALLOC-PTR ( n -- ptr cell )`. Existing byte allocation words immediately
weaken that base through the shipped core `BYTE-VIEW`, because their consumers
perform byte access. Add exactly one public entry:

```
MEM:ALLOC-BASE
   ( CAD-NUM:alloc-byte-len -- ptr cell CAD-NUM:alloc-byte-len )
```

It returns the same validated extent unchanged even when that extent is not
cell-divisible. Release remains the existing `MEM:RELEASE-BYTES` after the
caller weakens the base with `BYTE-VIEW`. BPE and MODEL-ASSET are the first
consumers and must delete their local pointer-view bridges.

Owner: package MEM in `lib/memory.f`; focused proof in `lib/memory-test.f`.
Acceptance: a real mapping supports first/last header-cell access, a
non-cell-divisible mapping preserves its exact extent and byte tail, byte APIs
still return `ptr u8`, and both forms release the exact mapping. Static negatives
reject treating an arbitrary `ptr u8` as `ptr cell`, widening `BYTE-VIEW`, role
swaps, and using a byte pointer for cell access. BPE and MODEL-ASSET tests prove
the exact returned extent is stored before owner acquisition and recovered for
release. A mutation that restores `MEM-ALLOC-PTR` to `ptr u8` must make
`ALLOC-BASE` fail to certify.
Focused memory, checker pointer, typed-local, package, BPE, and MODEL-ASSET gates
pass on the combined tree.

Forbidden: subspan, region, generation, alignment-evidence framework, runtime
modulus check, misaligned/result union, new error, new `TRUSTED:`, `MEM:BYTES`
duplicate, caller compatibility shim, docs, manifest, lint, or suite. This
shrunken outcome has no dependency on `habu-add-unique-bounded-527e05ca`.
