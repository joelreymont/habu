---
title: "Restore aot-payload-graph's authority-bits case"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T18:12:12.367712+02:00"
---

Problem: test/aot-payload-graph.f (SUITE aot-payload-graph, test/gate-stdlib-cases.f:1713) fails asserts 16 and 19 with rc 1 on the round-4 stack: after 'graph corruption applied: authority-bits' it prints 'graph mismatch got/expected: 0 -1', 'window: 79', then 'assert: expected 76 got 0' (F16) and 'assert: expected true got false' (F19). It passed in gate A on d40cc36d (gate-a/gate.log:734, PASS 19129ms), so a round-4 commit between d40cc36d and 4afe70e9 broke it, or the case depends on state the gate run had (the whitebox engine comes from ~/.cache/habu-build/build-cache-work-whitebox-engine-*). Seen identically by the r4-acapreq worker on 4afe70e9 with and without its change ($HOME/.cache/tmp/kestrel-r4-acapreq/ftout/graph-baseline.txt, test_aot-payload-graph.f.txt). Acceptance: name the first commit that fails (bisect d40cc36d..ddd2f7dd, each probe with that commit's own engine and a private HB_TMP) and the layer that is wrong (the corruption case, the graph check, the whitebox build or its cache key); fix that layer so the case refuses the corrupted authority bits as before; test/aot-payload-graph.f rc 0 on the fixed tree and rc 1 on the breaking commit; if a baked file changes, rebuild, g1 = g2 with .names, two-generation build. Not round-4 work only if the breaking commit is not in round 4 (say which). Files: decided by the bisect.
