---
title: Convert split-str batch ub
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T12:22:00.452130+01:00"
---

Problem: these files still carry the split-str marker: src/habu/main.f src/compiler/native/dict.f src/compiler/native/shadow.f src/habu/t0-place.f . Acceptance: no marker in them, check.f passes each, and the builds and suites that load them pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-ub.
