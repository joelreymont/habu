---
title: Locate origins across opaque top-level calls
status: open
priority: 2
issue-type: task
created-at: "2026-10-19T17:43:47.472873+03:00"
---

Coordinate-only origin navigation has no source-hash bypass. The conservative standalone index must defer at unmodeled dictionary calls, so unchanged src/core/layout-buffer.f and src/core/sumtype.f cannot currently supply authoritative prefix locations; removing the colon before SCRIPT-ARGC likewise makes the following SCRIPT-ARG-START location unavailable. Preserve these fixtures as deferred/missing-location checks in the private LSP adapter from e2a5e1f88b7c. Trusted does> clauses also have no index locations because the trusted scanner does not certify their bodies; ordinary trusted semantics remain unchanged. Future work must establish safe source bindings at the responsible verifier boundary without restoring hashes or guessing through opaque calls, retain the callback/collector/index and 72-byte FILE contracts, and exercise the real prefix and open-origin LSP conversations. Native-call modeling is deliberately outside the incremental qualification adapter. Unassigned; keep open until that capability exists.
