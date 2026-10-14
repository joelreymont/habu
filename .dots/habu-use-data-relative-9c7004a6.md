---
title: Use DATA-relative native addresses
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.775372+03:00"
---

Otto measures 1,703 absolute address chains, about 1,610 into DATA, consuming 20,156 bytes. Reduce their cost only with a design valid for shared DATA addresses and final relocation. Estimated 6–8 KB further savings are unverified. Acceptance: runnable unchanged-source applications and measured code/size reduction with correct address cases; focused E2E, native gate, convergence and Astra review. No codec.

The proposed x20 substitution is invalid: x20 selects the running task's private DATA region, while DATAADDR represents a shared address. test/data-address-codec.f SHARED-CASE already covers a worker updating its caller's cell and records the failed x20 lowering. Commit 5a1c658e5704 already shortened the valid DATA carrier from 16 to 12 bytes; Otto's later measurements include that saving. Capture and stripped linking rewrite fixed-size carriers and may relocate different allocations independently, so a compilation-time offset between two DATA addresses is not a stable fact. Independent Astra design found no smaller general lowering under the current ABI. Keep this outcome open for a measured successor design; do not dispatch an x20 rewrite or count the existing saving twice. A shared-base ABI or size-changing final-link relaxation is a separate design decision, not a local emitter patch.
