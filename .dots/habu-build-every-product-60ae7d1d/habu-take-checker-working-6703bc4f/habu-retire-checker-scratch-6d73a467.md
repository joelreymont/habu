---
title: Retire checker scratch before every image compare
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.458294+02:00"
closed-at: "2026-10-27T07:54:55.876667+02:00"
close-reason: "obsolete: master already retires scratch in IMAGE-TAKE and IMAGE-DIFF (tools/native-unit-object.f SCRATCH-RETIRE)"
blocks:
  - habu-record-every-loader-fb56cd0d
---

Lane 1: tools call CHECKER-REG:FILE-SCRATCH-RETIRE (today only OBJECT-OPEN, tools/native-definition-cache.f ~1340-1348) immediately before every IMAGE-TAKE and IMAGE-DIFF (native-unit-object.f SEGMENT-IMAGE, SEGMENT-CLOSE). Base: the edge lane (it moves those image calls). Claim: unassigned.
