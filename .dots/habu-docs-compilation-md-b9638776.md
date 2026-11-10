---
title: docs/compilation.md says a build loads the prefix unchecked
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:47:26.447105+02:00"
---

Problem: docs/compilation.md:14-17 says the engine's prefix files before src/core/check-hook.f load unchecked. In a build the builder's own checker checks them from the first row (src/core/check-hook.f:343-345 says so; a definition with a wrong effect placed at boot-files.f row 48 was refused: /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nwords-prototype.md). Only the cold prefix of an engine that was not seeded (src/habu/habu2.f EMIT-HOST-LOAD-PREFIX) loads them unchecked, and that path was read, not run. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the paragraph states which load checks the prefix and which does not, each with the fact that proves it. Files: docs/compilation.md. Verify: a wrong-effect definition in a prefix row under a build, and the cold-prefix path run or read. Depends: none. Ownership: docs/compilation.md. Claim: unassigned.
