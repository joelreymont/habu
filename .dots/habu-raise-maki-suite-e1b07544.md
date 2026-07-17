---
title: Raise maki suite-table capacity (ITEM-MAX 128 wall)
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T01:36:07.169691+02:00"
---

Discovered at the agent-loop landing (cfceb8be): lib/test/suite.f ITEM-MAX=128 and maki/test.f now registers exactly 128 suites - the NEXT TEST:SUITE addition fails E-TBL-BOUNDS at load. The capbud lane already had to aggregate four suites into one entry to fit. Work: raise ITEM-MAX (with the loud-fail preserved per the filemap-lint FM-BUF-CAP precedent - grow by constant, keep the bounds throw), or better: make the table growth-tolerant if the suite machinery allows a checked dynamic structure; update lib/test/suite-test coverage for the new bound; verify maki/test.f + gate suites unaffected. Small host change; no engine prefix. Files: lib/test/suite.f (+test), LESSONS if the aggregation-workaround pattern deserves recording. Ownership: test infrastructure.
