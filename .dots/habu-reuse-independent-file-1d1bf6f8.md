---
title: Reuse independent FILE units after edits
status: active
priority: 1
issue-type: task
created-at: "2026-10-14T16:55:46.372127+03:00"
---

Otto’s public436ff134 Dots reproducer (/tmp/otto-incremental.md and /tmp/otto-incremental-repro.sh) reports44.8–63.2s edits; an early main.f edit recompiles17 unchanged library units because FILE keys contain preceding build history. Fix at compiler/cache reference and dependency boundaries: relocate actual bound code/record/WID/DATA identities and record consumed contracts, values and executed producers. Preserve redefinition/private alias semantics and refusals. Acceptance: real early/middle/late app and engine edits reuse unchanged independent FILEs, execute edited behavior, preserve rejected programs, and measure complete invocation. Full native suite, uncached convergence and independent Astra review before landing.

At `76ad67a8`, both application lookup keys and range selection/restoration compare the preceding source chain. Stabilize application lookup using the pre-application build context, FILE-local source digest and existing parent/load-kind/path/root/occurrence identity. Before removing the application prefix guard, validate original spelling against the current resolution scope, incoming semantic scope/compiler mode and namespace creation/reopening decisions. Contract identity alone misses a new shadowing declaration or changed `using` scope. Unknown execution stays non-cacheable. Engine producer rules and explicit import signatures retain their own validation. Extend the existing stripped-path E2E before implementation with an unused root definition before requires, an early fixed value, a dependent file and a later independent file; verify exact hits/compilations, stable keys and runnable edited results.

Portable replay also rebases compiler counter deltas and CWORK allocations. The new CWORK offset descriptor must use scalar-cell storage: raw CREATE gives it DNAME-ALLOC but no DNAME-CELL, leaving adjacent numeric offsets subject to the existing bytewide mapped-pointer refusal. Use the existing variable scalar cell and its allocation at the owner; do not weaken the scanner or add another registry. Keep allocation/default relocation and generated accessor behavior in the existing CWORK E2E.
