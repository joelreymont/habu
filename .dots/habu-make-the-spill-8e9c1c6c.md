---
title: "Make the spill lowering's need fixpoint and frame-lane search linear"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:03:56.985941+03:00"
---

Problem: src/compiler/native/spill.f:947-952 F-NEED-FILL is a dataflow fixpoint (begin f n F-NEED-PASS 0= until) with F-NEED-PASS O(blocks) per pass and need propagating one block per pass, so a chain of 2k+1 blocks (the chain-scale spill-frame family) costs O(blocks^2); FRAME-ARG-PATH? (spill.f:671-683) recurses over successor block arguments with BMAX fuel once per block from F-LANES!, another O(blocks^2) per function. The combine lane's yardstick at load 12 read spill-frame slope 1595 with ns-per-handled rising 61344 -> 78949 -> 130929 (habu-make-spill-rewrite-ca192310 stays open on this). Acceptance: F-NEED-FILL uses a worklist or reverse-postorder so each block is revisited only when a successor changed; the frame-lane search is answered from a per-function map built once; tools/chain-scale.f spill-frame slope at or under 1.1 on a quiet box; the native spill suites and negatives green. Files: src/compiler/native/spill.f, tools/chain-scale.f. Verify: the yardstick, the suites, test/run.f. Depends: habu-make-spill-rewrite-ca192310. Ownership: hazel. Claim: unassigned.
