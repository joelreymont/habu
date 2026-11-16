---
title: "Load a body's computed required in quiet composition"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:16.637583+01:00"
---

Problem: quiet composition (src/habu/verify-source.f) never loads the file a body's `s\" x\" required` names, while discovery records it, so the composed check misses that file's words. Acceptance: quiet composition and discovery agree on the files such a body loads, shown on a fixture. Depends: none. Claim: unassigned.
