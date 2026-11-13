---
title: "Trace http-test's failed requests"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:02:40.876017+02:00"
---

Problem: a passing run of src/lib/net/http-test.f logs 'http: request rN failed, throw' lines. -9990 (E-BOOM, the test's fixture refusal, src/lib/net/http-test.f:85) and -3900 (E-JSON-READ-MALFORMED, src/lib/json/read.f:17) come from cases that provoke them; -5702 (E-BUF-STATE, a touch of a disposed buffer, src/lib/error.f:247) is unexplained, and a passing test may be masking it.
Acceptance: each request-failed line of a passing run is traced to the case that provokes it, -5702 included, and a defect behind -5702 is fixed where it lies.
Depends: none. Claim: unassigned.
