---
title: Grow the engine builder primitive registry with its contents
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-13T21:16:55.498414+03:00\\\"\""
closed-at: "2026-09-13T21:50:58.790973+03:00"
close-reason: Reviewed dynamic rows and name pool at d7184652; actual emitter audits all 197 rows, growth/reset/release tests pass at both tiers. Full private bootstrap succeeded in 341.180 s, producing hb-bootstrap-B1-registry SHA a4fc1996d67ae9a46d53d9cea58622a31291fc5d3f5cf9e1de58f2ce07a6c85a. Native is relocation failure is separately tracked by fa0c0c49; no accepted replacement engine claimed.
---

Owner: Cedar, .jj-ws/cedar-primitive-registry. Private bootstrap from 746ab84f passed target capture then refused primitive registry full (rc 76, 394.002 s, /tmp/cedar-family-stage-abi/native-bootstrap-B1-capacity.log). src/habu/habu1.f fixes registry capacity at 192 rows and names at 2048 bytes. Replace arbitrary build-side limits with existing checked dynamic storage; name references must survive growth. Preserve emitted dictionary layout and guarded helper metadata. Verify real source emitter independently of full bootstrap, row/name growth and reset, then full private bootstrap. This is distinct from runtime snapshot address table capacity.
