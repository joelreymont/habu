---
title: Reject a too-deep type before a walk runs out of stack
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T11:13:53.245143+01:00"
---

Problem: a field type nested past the depth limit stops with E-STACK-BOUNDS (loop stack in TFW-NODE at k=4800 for sdnest<n,…>; REQUIRE-POINTEES at k=6400 and SCH-BASE from k=9600 for ^-nested), while k=4000 gets the clean depth rejection: the walks recurse before the limit is checked. Also, some walk calls PF-NODE-KIND? (src/core/type-family.f) on a whole k-deep type when no generation is current and recurses k deep; its caller is not yet named. Acceptance: every input past the limit gets the depth rejection; a test at the old failing depths. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-deepcut
