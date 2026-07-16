---
title: Kernel artifact export for consumers
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-01T22:32:21.894725+02:00\""
---

Claim: agent=kexport workspace=.jj-ws/fable-kexport (lane resumed 2026-07-16
after its blocker habu-structured-kernel-abi-45c03544 landed at 8dfd1121; the
manifest derives from the KABI record).

No packaging story: habu runs only from its repo tree via bin/hb; an external project (a Zig-build consumer) cannot depend on checked-Habu output. Kernel roadmap explicitly targets running inside an external consumer's existing CUDA context (docs/ptx-sketch.md) yet there is no .ptx artifact export, no manifest (kernel name, param ABI span->(base,len,align), launch geometry, sm target, content hash), no build-step integration. Fix: 'hb kernel-export' emitting PTX + JSON manifest as versioned artifacts, ABI documented as a contract (ptx-sketch.md:361-375), example Zig consumer wiring. Prereq for any real habu-as-dependency use.

BLOCKED 2026-07-16 (kexport lane, honest stop): the manifest's param ABI has
no structured source - it is hand-encoded 3x (baked KERNEL: checker parse,
unexposed; hardcoded CG-ENTRY/CG-PARAMS strings lib/ptx/cg.f:57-69; hand
byte-offsets tools/ptx/cuda-launch.f:72-76). Deriving it as this dot requires
means the record must first exist: capability dot
habu-structured-kernel-abi-45c03544 (same lane builds it first; this dot
re-activates on top). Design scoped in the lane report: manifest v1 schema
(schema/version, name, target, ptx_version, address_size, block,
grid_derivation, params with per-kind lowering + dedup_key + source tags,
param_slots flat launch layout, ptx_sha256, manifest hash); reuse
lib/json-write.f JW-*, lib/content-key.f CK-*, PTX-CAPTURE, WRITE-ALL; new
lib/ptx/kernel-manifest.f + tools/ptx/kernel-export.f + tests + FILEMAP rows;
docs/ptx-sketch.md item 3 becomes the versioned contract; examples/
kernel-consumer/ Zig wiring (descriptive external-consumer example). Claim
released pending the capability; lane continuity noted in 45c03544.
