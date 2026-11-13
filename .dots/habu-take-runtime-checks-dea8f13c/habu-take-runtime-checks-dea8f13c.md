---
title: Take runtime checks out of default code
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:21:15.582339+02:00"
---

Problem: Joel 2026-11-13: "there should be no runtime checks, we should be checking things statically as much as possible. we can generate instrumented code with bounds and other checks ON DEMAND but not by default". Default code runs the store guard on every !, c! and +! (54M calls a warm engine build), a bounds check on every dynamic-buffer access (31M) and other checks. Acceptance: default engines and programs run no check that guards against a program error; each is a static check or exists only in code instrumented on demand. Depends: none. Claim: unassigned.
