---
title: Bind the DEFTYPE render check to the rendered field
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T13:09:47.016148+02:00"
---

Problem (review 312): test/engine-suite.f DEFTYPE check 'renders by name, not as ?' (after 9cac0fa3: the T-HAS? search on LOCJ-BUF near :1499-1503 of that tree) looks for 'frame-idx' anywhere in the E-MISMATCH JSON packet, and the packet echoes it in definition_source/declared_effect_source, so a renderer that printed '?' in expected/actual/inferred_effect would still pass. Acceptance: the needle binds to a rendered field (for example the actual/expected value), a mutation of the renderer to '?' fails the check (seen failing), and the check passes on the current engine. Files: test/engine-suite.f.
