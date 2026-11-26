---
title: Record every code span exact on both arches
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T10:06:48.965126+01:00"
---

Problem: src/habu/code-span.f keeps a legacy span (whole ARM64 instructions, the body before a final 4-byte slot, INSN-BYTES) beside the exact one (bit 31). Only arm64 primitives write it (src/habu/link-data.f PRIM-LEN: x86 EXACT, arm64 size-4); T0 opens every emission with ret 0 (src/habu/t0-emit.f NEMIT-OPEN), so CODE-SPAN:RECORDED's ret path and the RET-BYTES plumbing carry nothing, and x86's legacy branches (kernel-x64.f:1040, :3262, prof-x64.f:175, crash-x64.f:161) are never taken. One arch's convention in a shared layer. Acceptance: every recorded span is exact on both arches; INSN-BYTES, the legacy branches, VALID?'s legacy case and the ret parameter are gone; fixpoint on both; engine, native-gate-debug, engine-stack-debugger pass on both. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-span.
