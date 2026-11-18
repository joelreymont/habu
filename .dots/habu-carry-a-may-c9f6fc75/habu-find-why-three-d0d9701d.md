---
title: Expect the c9 rejections in xt-cell and ndict-binding
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:25:03.321804+01:00"
closed-at: "2026-11-18T09:50:36.289036+01:00"
---

Problem: under the c9 checker test/xt-cell-test.f GC2-GC4 (catch of a typed or deferred quotation) now read stale cells and are rejected E-STALE-READ, as the settled catch rule says; test/ndict-binding.f RETIRE-BINDING, an immediate word reaching undefine (CTOR-WORD?-XT states nominal), is rejected as writing, so no checked immediate word can retire a binding. Acceptance: xt-cell expects the rejections; ndict-binding drops the immediate retirement case; both entries pass with the c9 engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9t1.
