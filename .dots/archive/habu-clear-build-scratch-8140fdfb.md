---
title: Clear build scratch from saved apps
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T10:17:22.226997+03:00"
closed-at: "2026-10-21T11:45:17.192373+03:00"
close-reason: Remove invocation globals and clear owner scratch at existing capture boundaries, including logical-immediate state, obsolete family diagnostic pointer and CHX-BK. Independently reviewed. Edited/warm/fresh executable equality passes in existing E2E and real Maki d5c2 caller; native gate 486/486, full engine edit matrix and five byte-identical native generations pass.
---

Matched Maki602e32c3 source and sealed871ab298 engine produce identical cold/fresh images but a warm-hit image differs in four non-signature bytes. Owners are checker ISQ, TFAM TFW-BOOT[1], A64ASM RORE-R and LROT-ELEM; code/dictionary/framing are equal. Remove invocation scratch globals where ordinary locals express their lifetime, and clear the existing TFW boot scratch at its owner capture reset. Preserve public graph/type behavior and required owner/capture metadata. Acceptance: same source/output path/maker with cold, warm-hit and fresh-cache executable equality, runnable focused E2E artifact, independent Astra review and matching native/edit/convergence qualification. Keep this distinct from stale-family cache admission dc51595a.

Follow-through also attributed logical-immediate LELE/LIE/LIONES scratch and the obsolete REG-AOT-ERROR-A pointer after expected family invalidation. Their existing strengthened E2E now passes edited/warm/fresh executable equality. Frozen migrated Maki d5c2d976 on matching 0b85b1fa/06a989d5 exposes one remaining logical difference: CHX-BK at DATA4647608 is15905 cold/fresh versus2232 warm. All application builds exit0; cold and fresh hashes agree, while warm differs only in this cell and derived signature bytes. Trace and remove this index-rebuild cursor at its owner; retain the actual caller comparison before restarting broad qualification.
