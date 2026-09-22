---
title: Report DATA usage at build and name the ceiling
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T23:47:54.229362+03:00"
---

Problem: Linux's host layout reserves a 32 MiB DATA mapping, while the logical DATA window margin was invisible in hb-build output. Measurement on the current engine is 8,371,104 bytes of logical span against the 33,554,432-byte host ceiling; the old "two dictionary copies" explanation was wrong (the image-size walk accounts for one restored span). Acceptance: hb-build and a direct AOT link print `used / DATA-SIZE` with free margin, the JSON report carries the same pair, and docs/native-applications.md distinguishes logical DATA span from sparse file bytes and host layout capacity. The existing DP allocator remains the responsible layer for refusing a source that cannot fit its fixed mapping. Files: src/habu/aot-lib.f, tools/hb-build-report.f, tools/hb-build-lib.f, tools/image-size-lib.f, docs/native-applications.md, test/. Verify: the current engine's hb-build JSON parses `data_usage`; direct AOT output carries its data line; test/run.f. Depends: none. Ownership: AOT linker and build report. Claim: agent=alder workspace=.jj-ws/alder-data-usage.
