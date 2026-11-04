---
title: "Let PG:NOTIFIED? wait past one readiness poll"
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T14:25:21.358114+02:00"
---

Problem: PG:NOTIFIED? (src/lib/pg.f:1555-1566) refuses a wait past WAIT-MAX ($7FFFFFFF ms, :110, private) with E-BOUND, so Maki's PROJECT:WAIT (maki src/project-job.f:445-456), which Joel left uncapped (Maki DECISIONS.md 'Limits', 2026-11-04), fails past ~24.8 days and cannot clamp to a private constant. Relayed by tim 2026-11-04. Acceptance: NOTIFIED? takes any ms >= 0 (E-BOUND only below 0); the deadline saturates instead of overflowing; NOTIFY-WAIT polls at most WAIT-MAX per readiness wait; comments at :44 and :1551 follow; pg-test waits with the largest cell bound and a notification ends it. DEADLINE! unchanged. Files: src/lib/pg.f, src/lib/pg-test.f. Verify: bin/hb --load test/db/pg-cluster.f -- src/lib/pg-test.f. Depends: none. Ownership: pg.f. Claim: agent=worker workspace=.jj-ws.noindex/carl-pgwait.
