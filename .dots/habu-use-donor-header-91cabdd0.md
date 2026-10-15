---
title: Use donor header during FILE cleanup
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-15T10:24:08.860910+03:00\""
---

Combined reviewed source 152c5f2d (private incremental a17 plus master0657c5bc dictionary/layout changes), built by matching a17 native donor SHA2cd422e6, reaches src/lib/image-lifecycle.f then reports address-cells: invalid storage header. Tim donor-header scope covers final target preparation, while incremental FILE PREPARE-EXPORT also invokes target CHECKER-STORAGE-PREPARE during LOAD-TARGET. Identify and scope the actual live donor address header at every target cleanup boundary; preserve target capture header and failure restoration. Reproduce with the existing native build and layout-transition path using a FILE-capable older-layout donor, obtain independent review, and verify ordinary plus compiler-bundle capture. Preserve header refusal; no value heuristic or COLD-ONLY workaround.

Candidate a771ee31 passes the older-layout native build, but review rejects its namespace-presence test: ordinary application FILE exports retain the product ADDRESS-CELLS namespace while its private helper name is stripped. Restrict the scope to a replacement owner actually published in the current target record window; do not retain extra private names or catch lookup errors. Existing application build paths must continue to work, and a published replacement must still require the exact in-window helper.
