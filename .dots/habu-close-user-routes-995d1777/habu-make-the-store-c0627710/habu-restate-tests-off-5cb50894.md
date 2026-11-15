---
title: Restate tests off checker words going private
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:04:25.529026+01:00"
---

Problem: 223 test sites in 37 files call checker words store-feature2 S4 makes private (census /tmp/carl-s8/sites.tsv). Acceptance: each site states the same thing through source with surface words (CHECK:SCOPED, the candidates, CHECK!) or is dropped where another test covers the behaviour; the touched suites pass on today's engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-t8. After the wraps land, tests calling surface words by their global spelling (REPORTED-ERROR?, CHECKER-DEF-STOPPED?, CHECKER-PARSES-ROW and others) are requalified CHECK:NAME.
