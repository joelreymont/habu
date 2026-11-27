---
title: "Restate a range's value the person dragged away from"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T14:47:19.553432+01:00"
---

Problem: src/lib/browser/page.js update() (:506-515) sets a range's or text's el.value only when the row's value differs from the one it stated last (values map); a person's drag moves el.value but not that map, so when a module does not take a set and sends the same value, the thumb stays where the person left it, out of step with the module (tim's request /tmp/tim-habu-1127.md). Acceptance: a range row at v, dragged to w, then a toolbar record still stating v shows v. Depends: none.
