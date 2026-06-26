---
title: Clean engine/imgdump comments
status: closed
priority: 3
issue-type: task
created-at: "\"2026-06-25T12:19:43.587916+02:00\""
closed-at: "2026-06-25T22:29:43.475475+02:00"
close-reason: "completed: added definition-local stack effects to test/engine-suite.f helpers, moved tools/imgdump.f effects before locals, renamed h. to H., corrected E-NAME effect, and validated with bin/hb test/engine-suite.f, focused Linux imgdump load, test/gate-stdlib.f, and full native gate."
---

Finding F24. Evidence: docs/factorization-review.md:52; test/engine-suite.f:7 and tools/imgdump.f:63. Root cause: test/dump definitions lack stack-effect comments and some project words are lower-case. Fix: add comments before locals and rename project words to uppercase. Why: tests and tools are part of the checked Forth surface. Validate with engine-suite, imgdump-test, lint gates, and full native gate.
