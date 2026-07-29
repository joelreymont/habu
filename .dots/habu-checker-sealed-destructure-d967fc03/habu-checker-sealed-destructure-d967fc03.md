---
title: "Checker: sealed destructure and linear UNMAKE"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:00:47.469920+02:00"
blocks:
  - habu-seal-validated-model-7cd84aa3
  - habu-seal-gpt-2-def7cd75
  - habu-seal-model-provenance-21258c22
---

Campaign only; do not dispatch this parent. Public `UNMAKE` exposes private proof
tokens from validated configuration, layer identity, and provenance values.
The children add explicit `DESTRUCT public|owner` family metadata, parse it with
live/replay parity, implement the owner-only `destructure family` form, remove
public `UNMAKE` publication for owner-policy structures, and migrate each
affected inference authority separately. The form is a checker/compiler
operation that resolves only the active owning package and lowers to the
product-layout no-op; it is not a callable word or a caller-name heuristic.
Close this parent only after all three real forgery fixtures reject and their
production suites remain unchanged.

Recorded gap, measured 2026-07-28, needing a decision before this parent
closes: a FOURTH forgery surface exists in the tree with no migration leaf.
`maki/infer/gpt2-mapped-test.f:399-415`, inside `package GPT2LOAD-OUTSIDE-TEST`,
pins three deliberate ACCEPTs against `GPT2LOAD:gpt2-model` through the
generated `GPT2LOAD-GPT2--MODEL:UNMAKE`: the destructor extracts a real model's
store; the inner store can be released behind the model's back; and a real
model can be rebuilt with an invalid layer count of 99. The candidate at
`:409-411` is the rejected control proving the proof token still blocks a model
built from nothing. The rationale block at `:378-389` says that when this
capability lands those three tests fail, and that the failure is the signal to
delete them and retire the caveat in the `gpt2-load.f` header — so the pins
were written expecting coverage that the three named migration leaves (MDLCFG,
GPT2TENSOR, MODELPROV) do not provide. Either a fourth leaf migrates
`GPT2LOAD:gpt2-model` to `DESTRUCT owner`, or this parent's closure condition
is amended to state why that surface is deliberately left open. Do not close
the parent while the discrepancy stands.
