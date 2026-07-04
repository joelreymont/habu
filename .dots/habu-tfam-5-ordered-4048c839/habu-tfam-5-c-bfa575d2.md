---
title: "TFAM 5: C-quote/dot-quote loader + dynamic-span fail-closed"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:53:59.626268+02:00"
---

Unsupported string openers (C\" / .\") before a loader word must reject fail-closed instead of being replayed as a different source string (census sec1: they have no loader path today). Stack-string loader forms record the loader word's call-site span plus a path-origin classification; if a tool requires byte-exact path-expression spans and only a dynamic stack value is available, reject fail-closed. Add C\"/.\" loader-form negative fixtures. Depends on event-log + span-capture dots.
