---
title: Train N-block stacks
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-20T22:41:31.089429+02:00\\\"\""
closed-at: "2026-07-21T07:41:15.418445+02:00"
close-reason: "Landed in stack cb1e4cae: 4- and 12-block GPT-2 stacks now actually TRAIN, tied (decision made on data: tied final milli-CE 9/10 vs untied 14/494 - untied collapses at depth because the embedding loses the head-path gradient; honest init sc=1/sqrt-d). Generic any-depth tied trainer over flat slot-indexed weight/moment tables reuses the 1-block primitives. Locks: 4-block 1784->9, 12-block 1839->10 milli-CE over 16 steps, strictly monotone, run-twice bit-identical; exhaustive 60-slot gradient enumeration at 4 blocks (not sampled); falsification-verified. Zero capacity raises (723-node backward fits EX-NCAP 1024). Wall/step measured: 0.28/0.52/1.01/2.94 ms at L=1/2/4/12 - linear, ~0.24ms marginal per block, no wall at 12"
---

Training is proven only to 2 blocks (GBLK2 1783->1661 mCE, weak decrease); 4/12-block are capture + SAMPLED gradcheck + backward-build only (gptblock-attn-test.f:510-525, 12-block = 723 IR nodes). Extend to actual 4- and 12-block training with meaningful loss reduction, run-twice locked, exhaustive (not sampled) slot-gradient enumeration at 4 blocks minimum; record measured wall/step so the training-scale wall is data, not vibes.

Claim: agent=nblock workspace=.jj-ws/fable-nblock machine=spark (owns extending 4/12-block from capture+sampled-gradcheck to actual training: maki/examples/nanogpt/gptblock-attn-test.f N-block sections + any capacity constants via the coordinated-raise discipline)
