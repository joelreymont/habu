---
title: Convert split-str batch ua
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T12:22:00.315815+01:00"
---

Problem: these files still carry the split-str marker: src/core/arena-pages.f src/core/sha256.f src/habu/intern.f . Acceptance: no marker in them, check.f passes each, and the builds and suites that load them pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-ua.
