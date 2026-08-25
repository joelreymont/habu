---
title: Add shared inference model enums
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T16:06:15.012663+02:00"
closed-at: "2026-07-26T08:59:10.778018+02:00"
close-reason: "Implemented, reviewed, merged: landed as abc28c2baa7f (Add shared inference model enums, package MODEL), an ancestor of master@origin, as amended by the rev-4 inference leaf redesign: four enum families (gpt2/llama architecture, position, normalization, activation); the dtype family was deliberately deleted because MAKI:dtype in maki/tensor.f is the sole dtype authority - the module header records that decision. model-config consumes the MODEL types (MDLCFG landed d75a3846e5f4)."
---

Why: normalized configuration and the compiled-pack manifest currently define or would define the same model semantics independently, permitting tag drift and translation bugs. Exact interface: new maki/infer/model-types.f opens package MODEL and publicly declares enum family {gpt2,llama}, dtype {float32,float16,bfloat16}, position {learned,rope}, normalization {layer-norm,rms-norm}, and activation {gelu-new,silu}; no parser, storage, JSON, or target identity belongs here. Both model-config and model-pack-manifest require this module, return these exact MODEL types, and delete local duplicate enums or tag translators. Owned result: the five canonical semantic enums and focused constructor, exhaustive MATCH, and cross-type rejection tests in maki/infer/model-types-test.f, plus FILEMAP rows. Acceptance: each variant constructs and MATCHes through the public package; checked negatives reject swapping any two enum families even when their runtime tags coincide; a mutation routing global ENUM back to the legacy definer fails; model-config and manifest dots name this prerequisite before resuming. Smallest check: bin/hb --load maki/infer/model-types-test.f. Depends: habu-cut-global-enum-56ca54e2. Ownership: maki/infer/model-types.f, maki/infer/model-types-test.f, FILEMAP.md. Claim: RELEASED 2026-07-23; candidate 661384c7 is preserved as evidence but must not merge because its plain ENUM declarations execute the legacy definer before the global unified cutover.

Amended at closure (2026-07-26): the landed module deliberately defines FOUR
enum families, not the five named above. The dtype family {float32, float16,
bfloat16} was deleted by the rev-4 inference leaf amendment: MAKI:dtype in
maki/tensor.f (package MAKI, public) is the sole dtype authority, and a second
dtype enum here would have reintroduced exactly the tag-drift this dot exists
to prevent. The module header records that decision.

Claim: agent=claude-solo workspace=.jj-ws/habu-model-types. Recorded
retroactively at closure; implemented during the solo-orchestrator shift after
the earlier candidate 661384c7 was rejected for running plain ENUM through the
legacy definer.
