---
title: "B3: bare-ptr signature message"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-27T13:15:33.507456+02:00\""
closed-at: "2026-06-28T14:48:10.874969+02:00"
close-reason: "Landed on habu master@origin fa12fd17 gate-green (warm 130412ms<=160000ms, fixpoint, 0 non-budget fails). Fires at offending site: 'habu: in <w>: ptr needs an element type'; positives compile; GDX-BARE-PTR-SIGNATURE fixture in all-strict slice."
---

Checker: bare 'ptr' in a signature should error clearly ('ptr needs an element type: ptr a / ptr u8') or default to 'ptr a', instead of the current cryptic downstream failure. src/core/checker.f type parse.
