---
title: Repair maki kv-cache-test on master
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T02:12:37.417139+02:00"
---

maki/infer/kv-cache-test.f fails 14 of its 17 cases on master's OWN tree with master's OWN fixpoint engine, so it is master's campaign red and not a merge break. Measured 2026-08-05 in workspace .jj-ws/habu-master-probe at master@origin 5bd7f325, engine rebuilt there to its own fixpoint (sha dfa6b42e9335bc5722d43e353035efbefb6328b9e1f504c12b954a7c7da0f91f, byte-identical over two install --force runs, self-check census 0 uncheckable / 0 rejected / 4148 certified). The failing set is byte-for-byte the same set the proofs/master merge tree produces. Case 1 'post-open operations require no host allocation' forks a child that runs KVT-NO-ALLOC-BODY after 1 MMAP-TEST:EXHAUST-CHILD; the child dies on a native SIGBUS and the parent sees rc 134 where it expects 0 (habu-crash dumps sig 0xa). Cases 3-15 then fail 'expected true got false'; cases 2, 16 and 17 pass. Establish whether the 13 later failures are collateral from the exhausted mmap state the first case leaves behind or independent, then fix. Note the closed dot habu-kv-cache-collapse-9253f193 accepted this suite as green on master on 2026-07-30, so the regression landed after that.
