---
title: Train N-block stacks
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:41:31.089429+02:00\""
---

Training is proven only to 2 blocks (GBLK2 1783->1661 mCE, weak decrease); 4/12-block are capture + SAMPLED gradcheck + backward-build only (gptblock-attn-test.f:510-525, 12-block = 723 IR nodes). Extend to actual 4- and 12-block training with meaningful loss reduction, run-twice locked, exhaustive (not sampled) slot-gradient enumeration at 4 blocks minimum; record measured wall/step so the training-scale wall is data, not vibes.

Claim: agent=nblock workspace=.jj-ws/fable-nblock machine=spark (owns extending 4/12-block from capture+sampled-gradcheck to actual training: maki/examples/nanogpt/gptblock-attn-test.f N-block sections + any capacity constants via the coordinated-raise discipline)
