---
title: "B3: bare-ptr signature message"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T13:15:33.507456+02:00"
---

Checker: bare 'ptr' in a signature should error clearly ('ptr needs an element type: ptr a / ptr u8') or default to 'ptr a', instead of the current cryptic downstream failure. src/core/checker.f type parse.
