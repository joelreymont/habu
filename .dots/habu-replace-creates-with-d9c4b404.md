---
title: Replace CREATES with checked CREATE DOES>
status: open
priority: 1
issue-type: task
created-at: "2026-06-24T20:44:18.162014+02:00"
---

Root cause: Habu exposes a CREATES marker for created-word effects instead of using regular Forth CREATE ... DOES> as the checker boundary. Fix: make the checker/compiler attach the created-word effect to DOES>, update tests/docs/gate DSL to plain CREATE ... DOES>, remove CREATES keyword handling and legacy trusted fallback. Why: hard cutover to standard Forth surface; no legacy syntax or fallbacks.
