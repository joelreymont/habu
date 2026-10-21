---
title: Clear build scratch from saved apps
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T10:17:22.226997+03:00"
---

Matched Maki602e32c3 source and sealed871ab298 engine produce identical cold/fresh images but a warm-hit image differs in four non-signature bytes. Owners are checker ISQ, TFAM TFW-BOOT[1], A64ASM RORE-R and LROT-ELEM; code/dictionary/framing are equal. Remove invocation scratch globals where ordinary locals express their lifetime, and clear the existing TFW boot scratch at its owner capture reset. Preserve public graph/type behavior and required owner/capture metadata. Acceptance: same source/output path/maker with cold, warm-hit and fresh-cache executable equality, runnable focused E2E artifact, independent Astra review and matching native/edit/convergence qualification. Keep this distinct from stale-family cache admission dc51595a.
