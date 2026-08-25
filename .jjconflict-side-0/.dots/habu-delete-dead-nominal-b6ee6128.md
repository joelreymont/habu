---
title: Delete dead nominal CAD scaffold
status: closed
priority: 2
issue-type: task
created-at: "2026-08-02T19:00:21.711329+02:00"
close-reason: "Landed as 86592daa0ec1."
---

Problem: the ten-file lib/nominal chain and five-file src/cad/effect chain total 1959 lines, have no production caller, and their own tests state that they are direct-loaded future substrate; tools/bootstrap-mirror-lint.f carries a safety exemption solely for this unused tree. Result: delete lib/nominal arena, path, binding, codec, row, builder, snapshot and their three self-tests; delete src/cad effect-types, effect and three self-tests; delete the bootstrap-mirror src/cad exemption; remove only the dead R8/effect sections and dependency claims from docs/effects.md and MODEL-CAD-V2-PLAN.md. Preserve historical archive notes. Keep FS:path and CAD-KIND:id-error because TYPE-FIXES-PLAN freezes them as HF/GPT2 and M12 product inputs. Add no replacement abstraction, tombstone, alias, manifest, lint exception, dot tree, or parked code. Ownership: exact zero-consumer file cluster and its live prose/exemption edges. Acceptance: live references to the fifteen deleted files and dead R8 surface are zero; bootstrap-mirror lint passes without the exemption; GPT2 pin/tensor, Maki, exact diff, and full gates pass. Checkpoint: baseline direct self-tests and bootstrap lint green; source graph proves the cluster closes only on its own tests and deleting the exemption before source deletion is the measured structural violation.
