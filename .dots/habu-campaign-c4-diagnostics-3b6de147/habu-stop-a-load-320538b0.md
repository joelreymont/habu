---
title: Stop a load that a 0 throw unwinds
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.129850+01:00"
closed-at: "2026-11-16T04:07:33.989208+01:00"
---

Problem: a top-level call that throws a computed 0 (`: Z ( n -- ) throw ; 0 Z`, /tmp/jerry-c4/zero/z.f) unwinds the load: the rest of the file never runs and hb exits 0 with no record, since BTHROW (src/habu/habu1.f:3065) unwinds a 0 and the load's catch answers 0 as at the end. Acceptance: that file prints one record placed at the call and exits 67. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w45zero.
