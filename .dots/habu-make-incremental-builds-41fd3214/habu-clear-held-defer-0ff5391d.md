---
title: Clear HELD-DEFER when a defer publish throws before commit
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:01:58.187164+01:00"
---

src/compiler/native/publish.f PUBLISH-PENDING-DEFER stores HELD-DEFER before COMMIT-OWNED; if NHOST:TAKE throws first, HELD-DEFER stays set and the next ordinary publish commits as a defer. Found by hscr (code read).
