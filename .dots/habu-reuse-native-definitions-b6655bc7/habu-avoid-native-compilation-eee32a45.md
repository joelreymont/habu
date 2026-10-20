---
title: Avoid native compilation of build CLI
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T16:31:37.964498+03:00"
closed-at: "2026-10-19T17:18:09.406324+03:00"
close-reason: Reviewed5ef26698 avoids native compilation of uncaptured build orchestration; forced warm self-build CPU18.74 to10.59 with identical reuse and products. Native-build-entry E2E now passes unchanged exit0 after the separate fixture repair, including explicit native scope and refusals. Final campaign qualification remains with b665.
---

The compatibility entry now keeps uncaptured orchestration at the caller's tier and selects native tier before ORIGIN only for explicit/preloaded source builds. The identical outer closure used 9.09 CPU seconds and 2,192 native compile dispatches at tier 1 versus 0.52 seconds and none at tier 0. Forced warm compatibility builds used 18.74 CPU seconds before and 10.59 after, with identical reuse counts and byte-identical executable/map pairs. The explicit-target boundary reports native ORIGIN and tier 1 unchanged. Independent Astra review is clear. After the separately reviewed unknown-code fixture correction, unchanged test/native-build-entry.f passes exit0 on the3868 engine and combined8bfeb812 tools (127.05CPU/221.36wall), including arguments, source-state, cold/cached scope and foreign-ABI refusals. Final integrated native and public qualification remain with the parent campaign.
