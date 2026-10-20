---
title: Remove response-retirement test race
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:22:03.751759+03:00"
---

Jerry reproduced HTTP gate F143 under load: LARGE-CONTENT-CASE and two adjacent cases in src/lib/net/http-test.f observe the response before the worker retires the request, so an immediate E-HANDLE assertion races valid server cleanup. Evidence is in jerry-gate2/full.log and jerry-http143 under the external temp directory. Inspect existing controlled handle-lifecycle coverage; remove a redundant cross-request timing assertion or synchronize to a real completion boundary, preserving meaningful handle refusal coverage. Do not hide the race by raising deadlines or restarting servers per case. Run focused HTTP behavior checks and independent review before landing.
