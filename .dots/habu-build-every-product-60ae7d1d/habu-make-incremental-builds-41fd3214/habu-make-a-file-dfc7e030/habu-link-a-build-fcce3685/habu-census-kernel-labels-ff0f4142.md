---
title: "Census: kernel labels a product link needs (S6 L1b)"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T16:41:28.925438+02:00"
---

Problem: S6 L1 and L3 need each arm64 primitive's entry offset in the kernel emission, the header cells the boot fills with kernel labels, and every other kernel address T0 code calls (HELPER-REGISTER), each reachable by a public word or named as private. Acceptance: a read-only report with path:line evidence; no tree edit. Files: none. Verify: the report. Depends: none. Ownership: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l1b.
