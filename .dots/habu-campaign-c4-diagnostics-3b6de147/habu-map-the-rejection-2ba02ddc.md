---
title: "Map the rejection writer's rooms at start"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:24:39.475017+01:00"
closed-at: "2026-11-16T04:07:35.500473+01:00"
---

Problem: src/core/rejection.f TEXT (:74-77) and SAVED-ROOM (:101) map their first rooms on first use (ARENA-ALLOC, src/core/arena-pages.f:9-10), so when memory runs out the first record, such as E-MAP's carried one, dies 76 'checker: arena mmap failed' instead of printing. Acceptance: under ulimit -v, a program whose storage request is refused prints E-MAP's placed record, rc 67; TEXT has no first-use test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w52rej.
