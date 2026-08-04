---
title: Reconcile Linux engine size rows
status: active
priority: 0
issue-type: task
created-at: "2026-08-04T06:42:35.444713+02:00"
---

Problem: canonical bin/hb --load test/run.f on master ab7daa452eee0706efe1f27f41151cf924cb4176 builds SHA-256 07d38ea2b198ce18df4381681ff94fa4c567dcba15f913978b3734d53af035d6 and fails the exact Linux CODELEN ratchet: measured 118420 vs committed 118428. The live region map proves compile/ops 2744 vs 2456 and dictionary-code 5016 vs 5000, both owned by JIT shift commit cda6ec6d5540507e2670bf9107769887671402ba; aot-seed 22156 vs 22468, owned by SHA rewrite be534d2a55310ed68264b4daeb56a2a356130864. Net CODELEN is -8, Linux floor distance is 3732, text pad is 364, and total file size remains 123072. Result: update only the exact Linux CODE-TEXT, FLOOR-DIST, compile/ops, dictionary-code, and aot-seed measurement rows plus one concise attribution comment in test/gate-size-attribution-test.f. No source behavior, gate, lint, process rule, framework, or other target row changes. Owner: SIZE-ATTR package in test/gate-size-attribution-test.f. Acceptance: the map rows sum exactly to 118420; the candidate map equals every committed Linux row; bin/hb --load test/run.f exits 0 on the exact repair tree; current functional and performance phases remain green; LINUX-TOTAL stays 123072. Smallest real red: bin/hb --load test/run.f on ab7daa45. Depends: none. Ownership: test/gate-size-attribution-test.f only. Claim: agent=codex-codelen workspace=.jj-ws/habu-reconcile-linux-engine-c909aabd.
