---
title: Print a warning once in check.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T13:23:51.476314+02:00"
---

A warning-only source (W-EFFECT-NOT-RECORDED) prints its warning twice under tools/check.f: once from the in-process stage and once from the run child; in prose mode the in-process stage's warning leaks as a raw JSON line ahead of the prose one (review 436, both reproduce on the pre-round-4 engine). Acceptance: one warning line per warning, in the mode's format, seen failing first.
