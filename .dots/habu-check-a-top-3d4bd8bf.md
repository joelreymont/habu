---
title: Check a top-level call that keeps a ticked word
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T22:54:13.084751+01:00"
---

Problem: 'TYPED-VARIABLE KV [ -- ]' ': EVC ( -- ) s" 1 drop" evaluate-closed ;' ': U ( [ -- ] -- ) KV ! ;' then "' EVC U" at top level, one per line: the load rejects E-MISMATCH at U (4:7, the quotation does more than the type that keeps it); tools/check.f --verify-only exits 0 with no warning. The same call inside a definition (': T ( -- ) ['] EVC U ;') is rejected by both. Acceptance: the check rejects the top-level call as the load does, with a test that fails before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tick.
