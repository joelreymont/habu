---
title: compilation.md names a word no file defines
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.576440+02:00"
---

Problem: docs/compilation.md :17-19 says only the cold prefix of an engine that was not seeded (`src/habu/habu2.f` EMIT-HOST-LOAD-PREFIX) loads the files before src/core/check-hook.f unchecked; no file defines EMIT-HOST-LOAD-PREFIX (rg -i --no-ignore, twins review). Acceptance: the sentence states what loads the prefix unchecked today, or is deleted if nothing does. Files: docs/compilation.md. Verify: every word the passage names resolves with rg. Depends: none. Ownership: docs/compilation.md :12-19. Claim: unassigned.
