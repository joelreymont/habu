---
title: Delete the preseed axis and entry selection
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:42.519338+02:00"
---

Joel 2026-10-26. --preseed-mode has no consumer beyond cache keys: delete it (tools/hb-build-lib.f 101-111,166-169,375-406,452-459,718-721,997-1007,1215-1217; aot-build-core.f:20-50; aot-link-arm.f:527-541; aot-x64.f:173-179; aot-common.f:256-260; object.f:459-465; object-image.f ENTRY-ROW); coverage stays in test/gate-aot-positive-lib.f:660-676. Also delete entry-word selection (only test callers: stripped-entry-lib.f:51-54, hb-build-stripped-test.f:254-266). Before habu-simplify-the-build-1af55694.

Lane: jerry, worker in .jj-ws.noindex/jerry-preseed; handoff ~/.cache/tmp/jerry-preseed-handoff.md. Bad-tag guard coverage moves to MAIN-based TRUSTED forge fixtures in test/gate-aot-positive-lib.f. Claim: jerry.
