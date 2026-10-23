---
title: "Keep resident TRUSTED: definers and AOT imports green"
status: active
priority: 1
issue-type: task
created-at: "2026-10-23T23:10:08.215797+03:00"
---

Regression from slnrrlyu (Scan TRUSTED: bodies at tier 0): test/certify-does-definer.f asserts 27-30 (resident trusted definer: expected -1/0/-1/1, got 2) and test/aot-payload-graph.f asserts 7-8 (fresh boot executes imported code: expected 0 got 70) fail on the stack and pass on master 74c5551b. Lane: jerry, worker in .jj-ws.noindex/jerry-t0resid (on sqzxqknl). Blocks landing of 3913bb32's stack.
