---
title: Drop the x86 quotation-local workarounds
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T14:26:07.186317+01:00"
---

Problem: src/lib/net/http.f:275 (RUN-HANDLER), :411 (RENDER-REFUSAL), :716 (CALL-TAKEN), :1180, src/lib/genio-core.f:15 (the RUN-*-OP words) and src/lib/net/http-test.f:196 (ITEM-ID cells) pass values through cells or the task because 'x86-64 refuses a quotation that reads a local'. The native x86-64 engine built from master 94d51867 runs quotations that read a local under execute, catch and finally (/tmp/jerry-p1/x86/qlocal.f and qcatch.f print 42 42 and 42 5 0 on pulsar), so that premise is false. Acceptance: a quotation that runs inside its local's block reads the local and its cell or task detour goes; one that outlives the block (handed to a task or stored) keeps its detour with a comment stating the escape rule, not x86; the http and genio suites pass on spark and on pulsar's native engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-qlocal.
