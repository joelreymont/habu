---
title: "Give lib/net/curl.f BUFFER: storage and loop verbs beside AIO's"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T17:25:03.840492+03:00"
---

Problem: lib/net/curl.f keeps a private ZERO-CELLS, (curl.f:551-591), the same hand-rolled zeroed storage lib/aio.f dropped for lib/string.f BUFFER: (78916dca), and its public LOOP-START/LOOP-STOP now read oddly beside AIO:START/AIO:STOP. Acceptance: the create/ZERO-CELLS, pairs become BUFFER: where the alignment allows (say why where it does not); decide CURL:START/STOP and rename the consumers (docs/curl.md, docs/threads.md, lib/net/curl-test.f) in the same commit, announced downstream as an API break; no behaviour change; lib/net/curl-test.f and the gate green. Files: lib/net/curl.f, lib/net/curl-test.f, docs/curl.md, docs/threads.md. Verify: rg for ZERO-CELLS, and LOOP-START in lib/net/curl.f answers nothing; suites green. Ownership: hazel. Claim: unassigned.
