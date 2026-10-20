---
title: Refuse full report buffers by name in json-only and aot-call-report
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T14:28:09.653625+02:00"
closed-at: "2026-10-14T19:34:45.581597+03:00"
close-reason: "Implemented in b8d6584a: json-only and aot-call-report full buffers throw named capacity codes with prior complete output retained; check-core already streams discovery records. Red/green capacity cases and affected suites passed."
---

Problem (review 342): tools/json-only-core.f:79 JSON-ONLY-APPEND dies (JSON-ONLY-E-IO) and tools/aot-call-report-lib.f:113-117 REPORT-ROOM dies 74 when their own report buffers fill; tools/check-core.f:954-958 CHK-DISC-LEX hands the all-errors core CHK-OUT-BUF (32 KiB, BUFFERS!) and rethrows, so an unterminated string whose token exceeds 32 KiB in --source-list discovery exits 67 with an uncaught -2901. Acceptance: each refuses by a named throw its caller reports with the tool's documented exit, or streams (CHK-DISC-LEX: '2 >FD CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS:STREAM!' and drop the OUT$ copy); fixtures seen failing first. Files: tools/json-only-core.f, tools/aot-call-report-lib.f, tools/check-core.f.
