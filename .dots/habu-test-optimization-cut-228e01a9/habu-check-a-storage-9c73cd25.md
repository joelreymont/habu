---
title: "Check a storage record's class against its reason"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T11:09:11.909980+02:00"
---

Problem (review 279 of throwrows 1671da61): diag-contract's GJA-STORAGE-CLASS? (tools/gate-json-assert-core.f ~:610-614) admits any of the three storage classes and GJA-DIAG-STORAGE never reads `reason`, though docs/repair-diagnostics.md:70-73 says the class follows the reason. The renderer's reason is one of seven fixed strings mapped to the class in one switch (src/core/render.f ~:1275-1290 STGR-NAME-WHY?, STGR-COUNT-WHY?, STGR-REASON$, STGR-CLASS$). Demonstrated: the pre-pass record for `4 TYPED-BUFFER JSTG:A:B n` (reason "more than one ':' in name") relabelled fix_storage_type passes diag-contract rc 0 ($HOME/.cache/tmp/kestrel-r4-rev279/c2/rec/gap-stg-name-as-type.err). Acceptance: docs/repair-diagnostics.md lists the seven reason strings under their class; GJA-STORAGE-CLASS$ reads `reason` and answers the documented class (an unknown reason refused as one the renderer does not write); GJA-DIAG-STORAGE refuses 'storage repair class does not follow its reason'; one negative row in test/gate-diagnostics-lib.f from that record, seen accepted first; test/gate-diagnostics.f rc 0. Files: docs/repair-diagnostics.md, tools/gate-json-assert-core.f, test/gate-diagnostics-lib.f. Base: after e252f511 (1671da61) lands.
