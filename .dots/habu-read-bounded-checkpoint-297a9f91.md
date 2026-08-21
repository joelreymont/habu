---
title: Read bounded checkpoint F32
status: closed
priority: 1
issue-type: task
created-at: "2026-07-27T19:03:12.621200+02:00"
closed-at: "2026-08-02T16:00:48.764136+02:00"
close-reason: Superseded by commit 5b0ebb, which deleted the WSTORE/GPT2LOAD path; retaining this task would resurrect the removed design.
---

Why: GPT-2 compute must decode F32 checkpoint elements without reading beyond
a validated tensor extent, and it must read them from a loaded model, not a
raw span. Habu exposes byte `c@` and cell `@`; cell `@` is invalid for the
final four-byte element because it reads eight bytes.

This controller was refrozen twice after review. The original single-leaf
design (a raw `ptr u8` reader in a new `GPT2` package with `MEM:WITH-BYTES`
seams and guard-page fixtures) predated the landed bounded-read chain and the
store-owned model. A five-leaf replacement that row-generalized the scoped
callback combinators was then proven unsound through the real checker: a
checked body may legally return the scoped pointer through the polymorphic
result row, so scoped-span callbacks cannot truthfully pin a pointer-escape
rejection, and generalizing them would open a public raw-pointer channel. The
sound composition is value-level - the store answers bounded value queries
and no span crosses any package boundary. Do not implement this controller
directly. The work is four child leaves:

1. `habu-return-typed-idx-6811f99f` - `GPT2TENSOR:SLOT` returns
   `CAD-NUM:index`; public `CAD-NUM:INDEX=`; exact SLOT consumers migrate;
   no cast anywhere downstream. Deliberately leaves WSTORE untouched.
2. `habu-carry-model-config-c9085fa1` - `gpt2-model` carries the validated
   `MDLCFG:mcfg` through `mapped-check-result.ready` and
   `copy-check-result.ready`; public `MODEL-CONFIG`; scalar copies deleted.
3. `habu-add-bounded-u32-9bd95c8c` - one atomic WSTORE commit: retypes
   `SLOT!`, its callers, and the row helpers to `CAD-NUM:index`; deletes
   `WITH-SLOT`, the parked-frame plumbing, and their `TRUSTED.md` rows; adds
   public `WSTORE:U32-LE@?` taking the typed slot index and a slot-relative
   offset; mapped arm delegates to `SAFET:U32-LE@?`, allocated arm uses
   `CAD-NUM:BYTE+` plus four `c@`.
4. `habu-add-model-owned-7423a1e3` - public `GPT2LOAD:TENSOR-F32@?` joining
   them: typed slot and element offset derived while the model is intact,
   one UNMAKE, `WSTORE:U32-LE@?`, one rebuild, `F32:WIDEN`; typed refusals;
   non-skipping fixture proof.

Leaves 1 and 2 run in parallel; leaf 3 follows leaf 1; leaf 4 joins 3 and 2
(leaf 1 transitive) and closes this controller.
`SAFET:WITH-MAPPING` stays unchanged; its pointer-lifetime debt
(`habu-checker-ptr-lifetime-f59d1e9d`) remains outside this critical path.
`maki/infer/gpt2.f` stays reserved for the forward pass. Evidence note kept:
rejected commit `578c3ff4` erased CAD roles and forged allocation provenance
in its guard test; it remains evidence only. No claim is active.
