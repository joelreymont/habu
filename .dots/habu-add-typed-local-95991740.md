---
title: Add typed-local diff lint
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T11:01:44.565322+02:00"
---

Root cause: a Forth file split can make pre-existing bare locals appear as newly introduced code, and the current commit gate relies on an agent-run `rg` scan rather than a Habu-native check. Fix: implement a checked Habu lint that reads a diff/source list, finds added `{:` locals tokens without `:type`, respects the documented role-preserving exception only through explicit annotations/comments, and wires it into the native commit/gate path. Validate with fixtures covering typed locals, `len` role locals, bare-local rejection, moved-file additions, and false positives inside strings/comments. Why: typed Habu must be mechanical; text rules alone are not strong enough.
