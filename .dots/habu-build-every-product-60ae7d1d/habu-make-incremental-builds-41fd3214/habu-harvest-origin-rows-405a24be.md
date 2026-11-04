---
title: Harvest origin rows across a sink switch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:08.067510+02:00"
closed-at: "2026-10-31T20:52:53.794800+02:00"
close-reason: "Landed in lane 11 4b54ed21: origin rows follow the sink at each child load and each install; native-runtime.f reused in the warm engine."
---

Problem: warm-diag.md R4: native-runtime.f runs ENGINE-ORIGIN:INSTALL and ARM mid-segment; ORIGIN-HARVEST refuses origin-sink changed; 7 defs compile live. Acceptance: every row harvested under the sink that recorded it; native-runtime.f reused. Files: tools/native-definition-cache.f. Verify: P0 on the lane. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-r4.md. Depends: none. Ownership: SINK, ORIGIN-HARVEST. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fr4
