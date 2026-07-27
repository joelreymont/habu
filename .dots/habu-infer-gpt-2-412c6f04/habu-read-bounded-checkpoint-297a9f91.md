---
title: Read bounded checkpoint F32
status: active
priority: 1
issue-type: task
created-at: "2026-07-27T19:03:12.621200+02:00"
---

Why: GPT-2 compute must decode F32 checkpoint elements without reading beyond
a validated tensor extent. Habu exposes byte `c@` and cell `@`; cell `@` is
invalid for the final four-byte element because it reads eight bytes.

Owner and interface: new package `GPT2` in `maki/infer/gpt2.f` owns private
`F32@? ( ptr u8 CAD-NUM:byte-len CAD-NUM:index -- option<r> )`. It first
computes `count = len / 4`; returns `NONE` when `idx >= count`; only then
computes `idx * 4`, assembles exactly four little-endian bytes with `c@`, and
returns `F32:WIDEN` inside `SOME`. Lengths below four, trailing partial bytes,
and very large indexes return `NONE` without a throw or address computation.

No public raw-pointer word, type, span, native load primitive, global scratch,
PTX change, or change to scalar `F32`. Write set:
`maki/infer/gpt2.f`, `maki/infer/gpt2-test.f`, `maki/test.f`, and
`FILEMAP.md`.

Production seam: the focused test reopens `GPT2` and calls the actual private
reader inside real `MEM:WITH-BYTES`. This is the deepest production path until
QKV exists; the QKV and public-forward leaves must re-prove the same helper
through their published paths. Checkpoint: the exact private word is currently
unresolved.

Acceptance: offsets 0, 1, 2, and 3; `len=8,index=1` succeeds;
`len=7,index=1` and `len=8,index=2^62` return `NONE`; byte-position mutations
change exactly their expected bits; signed zero, normal, infinity, and NaN bit
patterns pass through `F32:WIDEN`; sentinels prove exactly four bytes read.
Run the focused suite, exact owning load, both diff lints, file-map and
suite-coverage gates, then `maki/test.f`. Independent destruction review is
required before integration.

Claim: agent=codex-f32-read workspace=.jj-ws/habu-read-bounded-f32
