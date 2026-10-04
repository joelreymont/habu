---
title: Refuse malformed declarations in checker callees
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T02:50:56.855112+03:00"
---

Problem: dies in the pre-verifier's callees that malformed input reaches, measured by lane 553 (dot 44279554 part B, probes $HOME/.cache/tmp/kestrel-jerry-lexlocb/cd/summary.txt): src/core/checker.f:5328 rc 70 for `DEFLINEAR n`, `DEFLINEAR ptr` or `DEFLINEAR CKLIN` twice under --verify-only (default mode: the nominal pass refuses first, unlocated, 'check.f: bad nominal type'); checker.f:6112 rc 70 for `VALUE-RECORD ckvr x nosuchtype END-VALUE-RECORD` under --verify-only; checker.f:8916 rc 76 for `s" CKX" s" ( n -- " TRUST` or `s" ( zzz -- )"` in default mode (child exits 76, check rc 69; --verify-only already gives a located E-BAD-STORED-SIGNATURE). Lines are on 09299d10+e48885d4. Fix: each is a located refusal by code through dot 44279554's statement-throw record, one record in every check.f mode, and the nominal pass's 'bad nominal type' line is located too. Acceptance: each probe seen dying first, then one located record per mode; baked: rebuild, g1 == g2, two-generation build. After: 44279554 part B.
