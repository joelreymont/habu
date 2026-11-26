---
title: Convert split-str batch uc
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T12:22:00.640300+01:00"
---

Problem: these files still carry the split-str marker: src/lib/span.f src/lib/process-tree.f src/lib/c2-memory.f . Acceptance: no marker in them, check.f passes each, and the builds and suites that load them pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-uc.
