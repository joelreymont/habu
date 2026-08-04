---
title: Expose checked GPU span
status: closed
priority: 2
issue-type: task
created-at: "2026-08-03T10:42:48.256075+02:00"
closed-at: "2026-08-03T11:33:12.229008+02:00"
close-reason: Merged as 77f29d7151fc9c9d7ce6eb6839d4eeeb1a4adee1; root and fresh destruction ACCEPT as the direct long-term range-and-bind API; exact full maki, lint-libs/ptx-stdlib/ptx-toolchain, real gpu-buffer, typed-local, package, and dot gates green.
---

Why: GPT2:model must pass bounded subranges of its one persistent GPU allocation to kernel parameters without exposing a raw buffer pointer or length. Interface: GPU:SPAN ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer result<cuda-devptr,n> ). It binds the session context, validates [off,off+len) with the existing overflow-safe range rule, preserves both owners, and returns the device pointer at off only on success. Owner: maki/gpu-buffer.f and its focused test only. Dependencies: landed GPU buffer owner. Acceptance: exact start/end/zero-length policy, one-over refusal, bind failure, two buffers and sessions, and real device pointer arithmetic pass through the public entry. Forbidden: raw length/dev accessors, launch/module API, callback, registry, new owner type, compatibility, version, or suite enrollment. Smallest real owning-path check: bin/hb --load maki/gpu-buffer-test.f on DGX Spark.

Claim: agent=codex-gpu-span workspace=.jj-ws/habu-expose-checked-gpu-4b503309
