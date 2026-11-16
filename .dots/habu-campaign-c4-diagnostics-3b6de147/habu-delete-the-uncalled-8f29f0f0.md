---
title: Delete the uncalled buffered report modes
status: active
priority: 2
issue-type: task
blocks:
  - habu-grow-the-test-39a36e17
created-at: "2026-11-16T11:49:35.923905+01:00"
---

Problem: tools/check-all-errors-core.f BUFFERS!/OUT$ and tools/json-only-core.f JSON-ONLY-BUFFERS! keep a report in a fixed caller buffer that throws E-DIAG-CAPACITY when full; once test/gate-dictionary-lib.f CHECK-BUF-RUN streams its report (habu-grow-the-test-39a36e17), nothing calls either. Acceptance: both cores only stream; E-DIAG-CAPACITY is gone. Depends: habu-grow-the-test-39a36e17. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w214.
