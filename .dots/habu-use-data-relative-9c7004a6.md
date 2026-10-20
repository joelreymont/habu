---
title: Use DATA-relative native addresses
status: active
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.775372+03:00"
---

Otto measures 1,703 absolute address chains, about 1,610 into DATA, consuming 20,156 bytes. Reduce their cost only with a design valid for shared DATA addresses and final relocation. Estimated 6–8 KB further savings are unverified. Acceptance: runnable unchanged-source applications and measured code/size reduction with correct address cases; focused E2E, native gate, convergence and Astra review. No codec.

The proposed x20 substitution is invalid: x20 selects the running task's private DATA region, while DATAADDR represents a shared address. test/data-address-codec.f SHARED-CASE already covers a worker updating its caller's cell and records the failed x20 lowering. Commit 5a1c658e5704 already shortened the valid DATA carrier from 16 to 12 bytes; Otto's later measurements include that saving. Capture and stripped linking rewrite fixed-size carriers and may relocate different allocations independently, so a compilation-time offset between two DATA addresses is not a stable fact. Independent Astra design found no smaller general lowering under the current ABI. Keep this outcome open for a measured successor design; do not dispatch an x20 rewrite or count the existing saving twice. A shared-base ABI or size-changing final-link relaxation is a separate design decision, not a local emitter patch.

Astra selected final-link pooling of profitable repeated DATA addresses after
object remapping, keeping JIT/capture/FILE carriers unchanged. An inspection of
the held Dots binary found 1,322 repeated-target sites and an approximately
8,352-byte logical text opportunity; this is not a built or timed improvement,
and its current segment padding would hide that saving in file length.
Implementation must inventory authoritative address sites, preserve every
code target through shortened members and exclude interior references. One
immutable pointer pool uses LDR-literal or ADRP/LDR according to exact reach;
no new register ABI, codec or runtime decoder. Accept only after paired
unchanged-source artifacts show a real benefit without a repeatable workload
slowdown, all relocation/Task/exception/callback E2Es and required gates pass,
and independent review clears the coherent linker change.

The paired unchanged-source stripped application measures 141,684 baseline
logical __text bytes and 133,540 pooled bytes, a reduction of 8,144 bytes.
Both signed Mach-O files remain 165,372 bytes because segment padding absorbs
the text saving. The pooled application completed the stripped subjects,
including shared DATA, caught throw, Task exit callback, and both exit-hook
paths; the focused stripped-image E2E and 27,000-word >1 MiB reach E2E pass.
This does not establish a
representative workload timing result or the native/convergence gates.
