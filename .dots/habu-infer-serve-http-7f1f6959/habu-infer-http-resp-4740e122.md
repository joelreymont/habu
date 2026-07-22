---
title: Infer HTTP response typestate
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T13:24:33.291162+02:00"
blocks:
  - habu-infer-http-syntax-c88a2a5a
  - habu-add-bounded-host-b40b048f
---

Problem: a response must not represent illegal header or body phases, and chunked output must own its wire framing. Acceptance: define public phase-specific linear response types for status, headers, stream, and done over owned bounded host storage. Each public transition consumes exactly one phase and returns the next; no generic public response with runtime phase integers and no raw-pointer refinement. The header seal generates exactly one Transfer-Encoding: chunked field for streamed responses, rejects caller Content-Length or Transfer-Encoding conflicts, and enforces body-forbidden status rules. Status, reason, names, and values use HTTP-SYNTAX. Duplicate initialization, stale phase reuse, raw mutation, copied authority, illegal phase order, exact capacity, and failure atomicity are checked negatives. Files: response typestate package, focused tests, Maki manifests and FILEMAP. Verify: phase checker negatives, exact wire head, storage ownership, owning Maki slice, typed-local, package, host, filemap, trust, strict inventory, and dot gates. Dependencies: habu-infer-http-syntax-c88a2a5a and habu-add-bounded-host-b40b048f. Ownership: response construction, phase authority, and structural framing headers only; writer calls belong to the streaming leaf.
