---
title: Make warm engine builds byte-identical to cold
status: closed
priority: 1
issue-type: task
created-at: "2026-10-31T18:01:39.778033+02:00"
closed-at: "2026-10-31T20:23:16.393739+02:00"
close-reason: "landed: lane 11 as 4b54ed21 Reuse compiled files in incremental builds, 9a25b36f, a8bdf2af on master; gate at the lane tip: native-build rc 0, registry rc 0 1164 s, P0 cold 164 compiled, warm 162 reused/2 compiled, warm == cold, two-generation rc 0; after rebase over ef2bf183 the overlapping rows (check, check-verify, gate-diagnostics, check-all-errors, repair-packet, load-reject-diag, runtime-regression, native-file-frame) rc 0"
---

Problem: at lane 11 (lqwqmsnq a70c57c5, on race fix twzuqopk), P0 builds the engine twice with hb-build --engine from one HABU_OUT: cold (files reused 0, compiled 164) and warm (reused 162, compiled 2). The two hb files differ in one byte, file offset 3338649 (0x36 cold, 0x06 warm), inside the captured DATA heap's cell values (outside the code blob 212496+2131672); the other 32 differing bytes are the code signature. The maps differ only in image-sha256. The pre-f7 lane P0 had warm == cold. Workspace .jj-ws.noindex/carl-p0 (worker). Acceptance: the DATA owner named, the layer at fault fixed, P0 warm == cold.
