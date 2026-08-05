---
title: Carry exact GPT-2 config
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T17:13:41.372373+02:00"
blocks:
  - habu-delete-model-config-1c71a13e
---

Why: gpt2-model copies layer count and config identity even though the validated MDLCFG:mcfg passed to PREPARE is the sole geometry authority. Result: PREPARE stores that exact mcfg in its private prepared owner; CHECK-MAPPED and CHECK-COPY take no later config argument and move the same mcfg into their ready result; LOAD-MAPPED and LOAD-COPIED move it into gpt2-model. The model becomes { weights WSTORE:store, cfg MDLCFG:mcfg, proof model-proof } and public MODEL-CONFIG returns the same non-linear mcfg. Delete MODEL-LAYER-COUNT, MODEL-CONFIG-KEY, every later CHECK config argument, and every caller or fixture for those old shapes in the same commit. Discard arms release the prepared or ready owner and drop only its copied non-linear mcfg. No config is rebuilt from scalars and no trust is added. Owner: package GPT2LOAD config transport only. Production red: PREPARE can validate one config while a later CHECK supplies a different config, and cfgkey currently hides that split authority. Acceptance: mapped, copied, payload, weight-store, package, and exact-diff gates pass; the old accessors and CHECK arities no longer resolve; every successful model returns the exact PREPARE config; a mismatched later config cannot be supplied. Forbidden: storing only derived scalars, second config input, config comparison, public raw fields, forwarding accessor, version, or compatibility path.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `codex-gpt2-config` and workspace `.jj-ws/habu-carry-model-config-c9085fa1` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `maki/infer/gpt2-load.f:711` and `:719` still define `MODEL-LAYER-COUNT` and `MODEL-CONFIG-KEY`, and no `MODEL-CONFIG` word exists. The dot stays active and is free to claim.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
