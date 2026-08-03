---
title: Expose checked GPU span
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T10:42:48.256075+02:00"
---

Why: GPT2:model must pass bounded subranges of its one persistent GPU allocation to kernel parameters without exposing a raw buffer pointer or length. Interface: GPU:SPAN ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer result<cuda-devptr,n> ). It binds the session context, validates [off,off+len) with the existing overflow-safe range rule, preserves both owners, and returns the device pointer at off only on success. Owner: maki/gpu-buffer.f and its focused test only. Dependencies: landed GPU buffer owner. Acceptance: exact start/end/zero-length policy, one-over refusal, bind failure, two buffers and sessions, and real device pointer arithmetic pass through the public entry. Forbidden: raw length/dev accessors, launch/module API, callback, registry, new owner type, compatibility, version, or suite enrollment. Smallest real owning-path check: bin/hb --load maki/gpu-buffer-test.f on DGX Spark.
