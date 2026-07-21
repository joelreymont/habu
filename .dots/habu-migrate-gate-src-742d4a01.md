---
title: Migrate gate source-dump buffer to WITH-BYTES
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T12:08:03.001747+02:00"
---

Follow-up from the WITH-BYTES landing (b1044d80): the consumer scan found essentially no bounded single-scope external users of raw byte allocation - everything else is a process-lifetime arena. The one real candidate is the gate's full-source dump buffer (GE-DFULL-SOURCE in test/gate-engine-lib.f), deferred because it is gate-critical with an escaping buffer. Migrate it with per-call verification that the buffer genuinely does not escape the scope; if it does escape, record that as the proven answer and close - the arena pattern would then be correct as-is.

Claim: agent=gatesrc workspace=.jj-ws/fable-gatesrc machine=spark (owns the gate source-dump buffer WITH-BYTES migration or its proven-escape negative)
