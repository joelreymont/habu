---
title: Mark a built run in its body-source row
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:02:34.759931+01:00"
closed-at: "2026-11-16T04:07:35.489166+01:00"
---

Problem: src/habu/interpreter.f PLACE-RUN writes a built run's row with the plain buffer offset of the token that built it, so src/core/rejection.f tells a built run from a copied one by comparing their first tokens' bytes (SPELLED?); a built run spelled like its definer token is read as a copy and loses its `= word:` note. Acceptance: PLACE-RUN marks the row as built and the readers read the mark; SPELLED? is deleted; postpone-define and build-items keep their placements. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
