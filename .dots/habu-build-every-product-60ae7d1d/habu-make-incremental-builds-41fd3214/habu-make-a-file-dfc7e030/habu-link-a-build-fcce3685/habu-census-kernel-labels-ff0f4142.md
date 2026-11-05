---
title: "Census: kernel labels a product link needs (S6 L1b)"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T16:41:28.925438+02:00"
closed-at: "2026-11-05T16:51:29.729011+02:00"
close-reason: "census reported: primitive entries via ENGINE-PRIMS:FIRST-LABEL + A64ICODE LBLP (public), by name AOT-PRIM-INDEX; header-cell labels are globals; T0 kernel targets resolve by record except five fixed primitive xts and the callback thunk; saved .jj-ws.noindex/carl-dots/.out/carl-file/s6-census-l1b.md"
---

Problem: S6 L1 and L3 need each arm64 primitive's entry offset in the kernel emission, the header cells the boot fills with kernel labels, and every other kernel address T0 code calls (HELPER-REGISTER), each reachable by a public word or named as private. Acceptance: a read-only report with path:line evidence; no tree edit. Files: none. Verify: the report. Depends: none. Ownership: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l1b.
