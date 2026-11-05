---
title: "Census: emitting the arm64 kernel for a product (S6 L1a)"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T16:41:28.911108+02:00"
---

Problem: S6 L1 (design r2 D1) puts the arm64 kernel emission at the head of a product's code; before its brief we need to know whether a running hb can assemble it by calling habu2.f's public EMIT-FORTH-IMAGE, its cost, and whether its bytes equal the running engine's own code head. Acceptance: a report with path:line evidence and measured wall/CPU; no tree edit. Files: none (probes in the workspace's .out). Verify: the report. Depends: none. Ownership: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l1.
