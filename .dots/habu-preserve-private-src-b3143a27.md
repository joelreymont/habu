---
title: Preserve private source labels in native maps
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T01:49:20.653704+03:00"
---

The existing hb-build-stripped-cache E2E fails its 64K-ALLOC-LEN source assertion with the private a452 donor: every private MEM word has source -, while public MEM words retain src/lib/memory.f. The builder asks for those labels after capture preparation sweeps private checker bindings. Collect the exact accepted paths before the sweep and retain their dictionary indices across both compiler-bundle captures; derive capture indices separately. Preserve alias and supersession semantics without name guesses. Verify the existing stripped-map fixture and compiler-bundle products, plus required checks and review.
