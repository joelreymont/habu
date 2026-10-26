---
title: Retire checker scratch before every image compare
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.458294+02:00"
blocks:
  - habu-record-every-loader-fb56cd0d
---

Lane 1: tools call CHECKER-REG:FILE-SCRATCH-RETIRE (today only OBJECT-OPEN, tools/native-definition-cache.f ~1340-1348) immediately before every IMAGE-TAKE and IMAGE-DIFF (native-unit-object.f SEGMENT-IMAGE, SEGMENT-CLOSE). Base: the edge lane (it moves those image calls). Claim: unassigned.
