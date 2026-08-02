---
title: Delete model semantic enums
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.541809+02:00"
closed-at: "2026-08-02T23:18:57.743850+02:00"
close-reason: "Landed at 105d2355: deleted unused model semantic enums and MDLCFG derived semantic accessors; exact full Maki and native stdlib/PTX gates passed."
blocks:
  - habu-match-gpt-2-ea837d37
  - habu-delete-unused-model-40bb32df
---

Why: MODEL family/position/normalization/activation/adapter enums and derived MDLCFG accessors have no product consumer after direct architecture matching. Result: delete model-types.f, its tests/suite entry, all five enum families, and derived FAMILY@/POSITION@/NORM@/ACT@ accessors. Owner: unused MODEL semantic types only. Production red: package-qualified XREF finds no remaining consumer. Acceptance: no removed symbol resolves; exact GPT-2 and Qwen MDLCFG arms still parse; Maki/native/package gates pass. Forbidden: replacement enum, adapter, registry, schema, version, compatibility record, or lint. Smallest owning check: model-config and full package XREF tests.

Claim: agent=codex workspace=.jj-ws/model-semantic-cut
