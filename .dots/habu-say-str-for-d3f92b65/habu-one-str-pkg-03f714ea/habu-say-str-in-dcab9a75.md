---
title: Say str in the Wasm harness rows
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:39:52.306102+01:00"
---

Problem: test/wasm/harness.f WASM-HARNESS:BUILD, VALID?, RUN, OUT$ and ERR$ still take or give ^u8 n, because their unmarked callers test/browser/{app,take,request,storage,navigation,echo,display}-test.f were outside the batch that unmarked harness.f; batch callers write `OUT$ >str`. Acceptance: the rows say str; every caller drops its split or >str. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasmharn.
