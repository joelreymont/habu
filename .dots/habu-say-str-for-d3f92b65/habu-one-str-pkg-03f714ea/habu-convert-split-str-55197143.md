---
title: Convert split-str batch ud
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T12:22:00.809976+01:00"
closed-at: "2026-11-26T12:41:12.755989+01:00"
---

Problem: these files still carry the split-str marker: src/lib/net/mcp-test.f test/fixtures/source-root-count.f src/lib/heap/wasm.f . Acceptance: no marker in them, check.f passes each, and the builds and suites that load them pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-ud.
