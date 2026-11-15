---
title: "Place PATH-DIE's records"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.139312+01:00"
closed-at: "2026-11-16T04:07:34.965084+01:00"
---

Problem: PATH-DIE (src/core/include.f ~:434) writes E-SOURCE-ROOT records with no place for BUILD-REFUSE, EXISTING-PARENT, ROOT-DIE, READ-REFUSE and the caller at ~:1887; only ROOT-SET places, through ROOT-DIR-DIE. Acceptance: each record stands at the path or the calling token by LOADER-PATH-AT's rule, and one word writes them all. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w41nul.
