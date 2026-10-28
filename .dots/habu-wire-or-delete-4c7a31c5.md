---
title: Wire or delete GE-CHILD-RUN
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.164750+02:00"
---

test/gate-common-lib.f:226-229 GE-CHILD-RUN has no callers; it rethrows an unguarded catch code, so with a zero code it would throw ENGINE-ERROR:THROW-ZERO on every success. test/preloaded-engine.f:124-127 says the child-timeout mapping goes through it, so that mapping is not wired. Either route the preloaded-engine child runs through it (guarding the zero code) or delete it and correct the comment. Source: THROW0 review 2026-10-28.
