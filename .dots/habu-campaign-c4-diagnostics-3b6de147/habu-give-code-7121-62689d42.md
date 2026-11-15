---
title: Give code 7121 one name
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.150807+01:00"
closed-at: "2026-11-16T04:07:36.066163+01:00"
---

Problem: code 7121, E-CHECKER-LAYOUT-BUFFER (src/core/checker.f:12750), is still thrown by the record-link grid words RECORD-LINK-GRID-ATTACH (:16073, :16083) and RECORD-GRID-AFTER (:16264-16265) on the checker's own table, and src/core/layout-buffer.f:10 aliases it as E-LAYOUT-BUFFER, which nothing throws. Acceptance: each test is deleted where its callers guarantee it, else dies 76 naming the invariant; the code and its alias are gone. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w53ckl.
