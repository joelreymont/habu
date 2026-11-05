---
title: MODEL logs a journal put that fails while journaling a refusal
status: open
priority: 1
issue-type: task
created-at: "2026-11-05T12:51:19.649478+02:00"
---

From stuart (Tender), 2026-11-05. src/lib/net/model.f JOURNAL-REFUSAL (:2614-2623) catches and drops a put fault (Tender's E-REPLIES-WRITE, later E-REPLIES-TAKEN) silently. Habu DECISIONS 2026-10-31 model clients: "make sure model library logs failures, errors, etc. dumping the textual reason." Acceptance: the call's own fault stays the one thrown; the put fault is logged as a LOG:ERROR "model" line naming the journal put and the put's code; a model-test case journals a refused live call through a put that throws and checks both. Files: src/lib/net/model.f, src/lib/net/model-test.f. Ownership: carl.
