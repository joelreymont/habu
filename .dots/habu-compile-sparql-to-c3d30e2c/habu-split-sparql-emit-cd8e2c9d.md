---
title: Split SPARQL emit.f by subject
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T16:11:51.692643+01:00"
---

Problem: emit.f is over 4,000 lines with a hand-made item union and a defer for load order alone. Acceptance: walks, query level and base are their own files and item is an ENUM. Depends: every other emit.f lane.
