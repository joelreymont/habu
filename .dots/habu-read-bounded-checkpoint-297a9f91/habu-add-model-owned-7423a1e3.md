---
title: Add model-owned tensor read
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T17:13:41.385140+02:00"
blocks:
  - habu-add-bounded-u32-9bd95c8c
  - habu-carry-model-config-c9085fa1
---

Why: nothing can read a weight element from a loaded model; this is the first
read on the model-forward critical path. Exact result: public
GPT2LOAD:TENSOR-F32@? ( gpt2-model GPT2TENSOR:tensor-id CAD-NUM:index --
gpt2-model option<r> ) in new file maki/infer/gpt2-read.f, which reopens
package GPT2LOAD and defines no private pointer reader and no raw-span helper
(maki/infer/gpt2.f stays reserved for the forward pass). Owner path frozen:
while the model is intact, run MODEL-CONFIG and GPT2TENSOR:SLOT (its E-CONFIG
and E-LAYER identity errors stay explicit contract errors, outside the typed
refusal path; SLOT now returns the typed CAD-NUM:index, consumed with no cast)
and derive the element-relative CAD-NUM:byte-off from the element index via
CAD-NUM:INDEX-BYTE-OFF with the named four-byte width, every numeric-result
arm handled, all before any UNMAKE; then UNMAKE the model exactly once inside
GPT2LOAD, call WSTORE:U32-LE@? on the store with the typed slot index and
offset,
rebuild the model exactly once from the returned store, and map some(bits)
through F32:WIDEN into option<r>. An out-of-extent index and every store-side
refusal yield none. No raw owner cells, globals, or parked state anywhere on
the path; no raw pointer or span at any boundary; expected failures are typed
results. PREPARE's existing E-ELEMENT-TYPE check already proves every model
tensor is F32, so no per-read dtype dispatch exists. Owner: package GPT2LOAD
in maki/infer/gpt2-read.f, whose direct requirements are exactly
maki/infer/gpt2-load.f, maki/infer/gpt2-tensor.f, maki/infer/weight-store.f,
maki/infer/model-config.f, lib/float32.f, lib/adt/option.f, and
lib/cad-num-arithmetic.f. The focused test is maki/infer/gpt2-read-test.f,
registered in maki/test.f and in exactly one slice, maki/test-core.f.
Acceptance is non-skipping: load a deterministic
valid GPT-2 checkpoint fixture through the real GPT2LOAD production path, call
public TENSOR-F32@? on an authoritative GPT2TENSOR:tensor-id, and compare
fixed independent bytes - a missing optional large artifact cannot satisfy
acceptance; also a wrong-config LAYER tensor-id E-CONFIG rejection, the
last-valid and first-invalid element indexes, an index overflow case, a
store-refusal none, identical values read from a real mapped load and a real
copied load of the same fixture, and mutants in the established
guaranteed-read style; suite registered in maki/test.f and exactly one slice;
full maki suite; both diff lints. Forbidden: raw span exposure, private
pointer readers, throws for data conditions, skipped-fixture acceptance,
second geometry authority, touching maki/infer/gpt2.f.
