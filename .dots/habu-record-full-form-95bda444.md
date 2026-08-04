---
title: Record full-form sites in the enum census
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T11:40:47.459917+02:00"
closed-at: "2026-08-02T16:48:00.667327+02:00"
close-reason: "Obsolete: authoritative ancestor 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8 deleted the enum-census core, CLI, tests, and baseline. Retention would resurrect the deleted enum-census and baseline architecture; no replacement tooling exists or is intended."
---

Coverage hole measured by the wave B4 lane: tools/enum-census-core.f:365 counts full-form ENUM sites and then skips them, so the byte-compared census baseline covers only compact sites. Every SUMTYPE the migration program converts to a full-form ENUM therefore leaves census coverage: variant-name, tag-order, or constructor-symbol drift in a migrated family is invisible to the census gate. The hole grows with every lane (5 full-form sites before wave B, 13+ after, all invisible). Behavior: the census records full-form sites in the baseline with the same per-variant facts it records for compact sites (name, tag, ctor symbol presence, kind, width, visibility), the baseline is re-recorded once with the exact expected delta enumerated in the change report, and the verify path byte-compares them like any other row. Coordinate with habu-make-census-scratch-787ddd48 (scratch-ordinal naming) so the two baseline reshapes land as one re-record, not two. Hostile fixture: a variant rename in a full-form fixture family must red the verify. Acceptance: census suite green, verify red on the hostile fixture, baseline delta enumerated. Owner: tools/enum-census-core.f. Dependencies: sequence with habu-make-census-scratch-787ddd48; land before the migration program FINAL lane.
