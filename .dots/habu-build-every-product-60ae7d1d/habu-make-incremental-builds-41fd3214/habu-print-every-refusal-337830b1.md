---
title: Print every refusal reason under cache diagnostics
status: closed
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:17.041654+02:00"
closed-at: "2026-10-28T21:59:01.213073+02:00"
close-reason: "Landed 748a856b: every reason prints under HABU_NATIVE_CACHE_DIAG; census 184 refused / 8 reused in carl-diag/.out/census.tsv"
---

Roots lane L0 (census .jj-ws.noindex/carl-roots/.out/roots.md): under HABU_NATIVE_CACHE_DIAG each file prints every refusal reason, not only the first, so each later lane's unlock count is known. Worker in .jj-ws.noindex/carl-diag (brief .out/brief-l0.md), with the all-reasons warm census. Claim: carl.
