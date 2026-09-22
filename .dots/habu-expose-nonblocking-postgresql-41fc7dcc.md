---
title: Expose nonblocking PostgreSQL progress
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T19:56:01.289016+03:00"
---

Owner Cedar. libpq calls are nonblocking, but the PG convenience operations wait in AIO:AWAIT and therefore park a Habu pthread. Acceptance: a dispatcher can start connections and queries, poll each for completion or an fd/readiness mask, and service multiple connections from one task without awaiting any one inside PG; existing blocking-style conveniences compose that surface. Preserve connection/result ownership, cleanup and diagnostics. Verification: one task starts a query blocked on another connection and releases it through that other connection; cancellation/close frees pending state. Tender dispatcher integration is a separate application slice.
