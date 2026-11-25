---
title: Accept a quotation a kept variable gave a call
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T23:10:29.003979+01:00"
---

Problem: ': U ( -- ) [: parse-name drop ;] KV @ 0 TIE execute ;' (with 'TYPED-VARIABLE KV [ -- ]' and ': TIE ( [ -- ] [ -- ] n -- [ -- ] ) 0= IF drop ELSE nip THEN ;', /tmp/jerry-p1/psd/c/m2.f) is rejected E-MISMATCH at TIE 7:41 by the load and the check, while m2b.f, the same with TIE's arguments in the other order, loads; the plan /tmp/jerry-p1/br/pass-plan.md infers that keeping reads a block state the caller's own kept variable gave the instance. Acceptance: m2 loads and checks as m2b does, in a test that fails before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-m2.
