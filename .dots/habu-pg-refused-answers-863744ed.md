---
title: "PG:REFUSED$ answers an earlier rejection after a later failure"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:32:56.085881+01:00"
---

Problem: OP-START (src/lib/pg.f) empties only a loss and keeps a rejection, PG:ENDED? closes without libpq's text, and a rejected BEGIN, COMMIT or ROLLBACK keeps none, so PG:REFUSED$ answers an earlier statement's rejection as this failure's; reproducer /tmp/tim-habu-refused-stale.f, report /tmp/tim-habu-refused-stale.md (tim, Maki's loss log lines wait on it). Acceptance: one text record per task, emptied by every operation's start and set by the operation's own failure, with cases in pg-rows-test.f for an ended connection, a rejected deferred COMMIT and a successful operation after a rejection. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pg-refused.
