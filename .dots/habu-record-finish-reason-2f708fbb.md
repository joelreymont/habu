---
title: Record finish_reason stop in MODEL journals
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T19:10:40.984849+02:00"
---

phil 2026-10-31: READ-FINISH (src/lib/net/model.f ~1631) keeps only a non-stop reason in FINISH-U; the recorder (~2178) writes finish_reason only when FINISH-U is nonzero, so every accepted generation records null and a row cannot tell stop from no reply or an embedding (tender DB control.model_call: 44 ok calls, all NULL). Acceptance: the record holds stop when the provider said stop (including a refusal that finished with stop), the kept reason when incomplete, null otherwise; model-test asserts the recorded field for stop, length and embed. Files: src/lib/net/model.f, src/lib/net/model-test.f. Verify: hb --load src/lib/net/model-test.f. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-fin
