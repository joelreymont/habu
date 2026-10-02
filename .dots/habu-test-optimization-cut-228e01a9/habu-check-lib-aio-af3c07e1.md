---
title: Check lib/aio.f without a duplicate family refusal
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T13:04:06.484146+02:00"
---

Problem (review 306): bin/hb --load tools/check.f -- lib/aio.f prints 'habu: bad newtype declaration 'ticket': duplicate family at 'ticket'' and 'hb: uncaught throw code 7102' (exit 67), before and after a4da213d; lib/aio.f:36 NEWTYPE ticket declares the family once, and loading the file is clean, so check.f's pre-pass registers it twice in one checker scope. Acceptance: reduce which check.f pass declares the family a second time; check.f on lib/aio.f gives the engine's verdict (rc 0 if the load is clean), seen failing first, with a case in tools/check-test-lib.f through the real CLI; no uncaught throw. Files: tools/check-core.f, tools/check-test-lib.f.
