---
title: Read bounded checkpoint F32
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T19:03:12.621200+02:00"
blocks:
  - habu-make-owned-release-79de2b5c
---

Why: GPT-2 compute must decode F32 checkpoint elements without reading beyond
a validated tensor extent. Habu exposes byte `c@` and cell `@`; cell `@` is
invalid for the final four-byte element because it reads eight bytes.

Owner and interface: new package `GPT2` in `maki/infer/gpt2.f` owns private
`F32@? ( ptr u8 CAD-NUM:byte-len CAD-NUM:index -- option<r> )`. It derives the
complete element count with `CAD-NUM:DIV-BYTES-FLOOR`, tests the index with
`CAD-NUM:INDEX-IN-COUNT?`, and derives the byte offset with
`CAD-NUM:INDEX-BYTE-OFF` before touching the pointer. Only the final pointer
addition may privately project `CAD-NUM:byte-off` to `n`; no length or index is
erased. The reader then assembles exactly four little-endian bytes with `c@`
and returns `F32:WIDEN` inside `OPTION:SOME`. Lengths below four, trailing
partial bytes, and very large indexes return `OPTION:NONE` without a throw or
address computation.

No public raw-pointer word, nominal-to-raw conversion, type, span, native load
primitive, global scratch, PTX change, or change to scalar `F32`. The one
private byte-offset projection is the audited machine-address sink. Write set:
`maki/infer/gpt2.f`, `maki/infer/gpt2-test.f`, `maki/test.f`,
`maki/test-core.f`, and `FILEMAP.md`. Register the inference suite in the
master `maki/test.f` list and in exactly one parallel slice,
`maki/test-core.f`.

Production seam and checkpoint: the focused test reopens `GPT2` and calls the
actual private reader inside real `MEM:WITH-BYTES`; before implementation the
exact private word is unresolved. Stop on the first representative definition
if typed CAD arithmetic, the package gate, or the actual candidate checker path
requires an unplanned cast or public interface. QKV and public-forward leaves
must later re-prove this helper through their published paths.

Acceptance: offsets 0, 1, 2, and 3; `len=8,index=1` succeeds;
`len=7,index=1` and `len=8,index=2^62` return `NONE`; byte-position mutations
change exactly their expected bits; signed zero, normal, infinity, and NaN bit
patterns pass through `F32:WIDEN`. Child-isolated left- and right-guard-page
fixtures use the real `MEM:UNMAP(ptr, byte-len)` range interface and prove the
reader touches neither byte outside its four-byte element. Candidate-routed
checker fixtures load the real `gpt2.f`, accept only the exact typed call, and
reject raw lengths/indexes, swapped roles, `alloc-byte-len` substitution, and a
raw pointer role. Comments, strings, or a copied validator cannot satisfy any
proof. Run the focused suite, exact owning load, both diff lints, file-map,
suite-coverage, and refine gates, then `maki/test.f`. Independent destruction
review is required before integration. Rejected commit `578c3ff4` is evidence
only; it erased CAD roles and forged allocation provenance in its guard test.
No claim is active.
