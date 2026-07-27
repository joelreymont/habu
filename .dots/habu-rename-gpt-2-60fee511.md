---
title: Rename GPT-2 tensor API
status: active
priority: 1
issue-type: task
created-at: "2026-07-27T19:23:40.703222+02:00"
---

Why: the predecessor package does not load anything. It defines the canonical
identities, names, shapes, orientations, and slots for tensors in a GPT-2
checkpoint. Its package and short type names hide that responsibility.

Owner and exact interface: `maki/infer/gpt2-tensor.f` is the sole production
file and package `GPT2TENSOR` is the sole owner. Public nominal types are
`global-role`, `layer-role`, `orientation`, `layer-proof`, `layer-id`, and
`tensor-id`; the layered `tensor-id` variant is `layer`. Public words are
`LAYER-ID`, `COUNT`, `COPY-NAME?`, `SHAPE`, `ORIENTATION`, `SLOT`, and
`FORMAT-ID`. Public errors are `E-LAYER`, `E-CONFIG`, and `E-SIZE`. Generated
constructor, matcher, equality, `MAKE`, and `UNMAKE` names follow those package
and declaration names.

Behavior and representation do not change: the same four global roles, thirteen layer roles, exact Hugging Face names, shapes, Conv1D orientations, configuration-authenticated layer IDs, slot order, overflow checks, and copy-out contract remain authoritative. All direct consumers change atomically to the new interface.

Owned write set: create `gpt2-tensor.f` and `gpt2-tensor-test.f` by renaming
their predecessors; update the currently misnamed checkpoint loader fixture,
test, and module, the sole direct fixture-helper consumer
`gpt2-alloc-test.f`, `model-provenance.f`, `model-types.f`, `maki/test.f`,
`maki/test-core.f`, `tools/refine-lint-core.f`,
`tools/enum-census-baseline.txt`, `FILEMAP.md`, `TRUSTED.md`, and only current
task or lesson text that names this exact tensor API. The separate dependent
loader-name leaf owns renaming the checkpoint loader files and package. Do not
alter checkpoint-load behavior here.

Forbidden: aliases, forwarding words, duplicate declarations, compatibility files, semantic changes, new tensor kinds, new validation, broad replacement of unrelated uses of bind, or keeping an old generated name reachable.

Production defect and acceptance: before the change, `GPT2TENSOR` and
`maki/infer/gpt2-tensor.f` are absent. After the change,
`bin/hb --load maki/infer/gpt2-tensor-test.f` and every direct consumer suite
pass through the renamed production package; exact symbol probes accept each
new type and reject every predecessor package, type, word, and generated
constructor; the enum census is regenerated from the real declarations; no
predecessor tensor-package or file name remains in current source, tests,
documentation, or task text. Exact package and typed-local diff gates pass.
This is a pure hard cutover and one compiling commit.

The allocation test is included only to rename its direct calls from the old
private fixture-helper names to `TX-NAME-LEN` and `TX-NAME-BUF`; no behavior
changes there.

Claim: agent=codex-gpt2-tensor workspace=.jj-ws/habu-rename-gpt-2-60fee511
