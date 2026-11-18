---
title: "Name the cell a kept quotation's parameter lacks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T08:09:07.092435+01:00"
closed-at: "2026-11-18T11:24:41.981461+01:00"
---

Problem: a quotation that keeps its parameter in a cell whose type lacks a fact the parameter's type states is rejected with the outer record: /tmp/jerry-nested/q2.f (MKK2's [: KV ! ;] stored in SV [ [ -- renders ] -- renders ], KV [ -- ]) and q8.f print 'this quotation does more than the type that keeps it states', found [ [ -- ] -- ], help 'state renders in the type that keeps it', though SV already states it: the found type hides the inner lack and the help names the wrong type. Acceptance: the q2 and q8 records name the parameter, the cell that keeps it (KV) and the fact KV's type lacks, and the help names KV's type or the parameter's; a test fails first. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-qdiag.
