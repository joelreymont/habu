---
title: Clear HELD-DEFER when a defer publish throws before commit
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:01:58.187164+01:00"
closed-at: "2026-10-28T17:17:29.902132+02:00"
close-reason: "carried to habu-publish-and-shadow-8ea8909d, which replaces COMMIT-OWNED and NHOST:TAKE"
---

src/compiler/native/publish.f PUBLISH-PENDING-DEFER stores HELD-DEFER before COMMIT-OWNED; if NHOST:TAKE throws first, HELD-DEFER stays set and the next ordinary publish commits as a defer. Found by hscr (code read).
