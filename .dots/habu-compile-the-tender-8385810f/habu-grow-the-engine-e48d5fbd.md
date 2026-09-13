---
title: Grow the engine builder primitive registry with its contents
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T21:16:55.498414+03:00\""
---

Owner: Cedar, .jj-ws/cedar-primitive-registry. Private bootstrap from 746ab84f passed target capture then refused primitive registry full (rc 76, 394.002 s, /tmp/cedar-family-stage-abi/native-bootstrap-B1-capacity.log). src/habu/habu1.f fixes registry capacity at 192 rows and names at 2048 bytes. Replace arbitrary build-side limits with existing checked dynamic storage; name references must survive growth. Preserve emitted dictionary layout and guarded helper metadata. Verify real source emitter independently of full bootstrap, row/name growth and reset, then full private bootstrap. This is distinct from runtime snapshot address table capacity.
