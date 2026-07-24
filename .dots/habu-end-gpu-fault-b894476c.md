---
title: End GPU fault test after contained fault
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T03:52:10.069000+02:00"
---

Why: maki/eval/device-fault-test.f deliberately triggers a real NVIDIA Xid 31 memory fault, then launches a second CUDA candidate and requires it to be green. Repeated full Maki runs on DGX Spark proved that the immediate post-fault launch can return EVN-DEVICE-FAULT even though the grader process survived. The test currently confuses process containment with immediate device recovery.

Owner: package EVAL owns the device grader regression. No public interface changes.

Exact result: in maki/eval/device-fault-test.f, first grade EDFT-CORRECT$ and require EVN-GREEN to prove the device and grader are healthy before the destructive action. Then announce the deliberate fault, grade EDFT-FAULTER$ in the existing fresh spawned child, require EVN-DEVICE-FAULT, and finish with parent-side reporting only. The deliberate fault is the final CUDA operation in this suite. Update the contract prose to claim only that the grader parent survives and reports the fault. Preserve the off-device skip, real fault, child isolation, journal annotation, verdict mapping, cleanup, and existing package owner.

Forbidden: retrying a candidate, sleeping, resetting or recreating the primary context as a guess, probing a magic CUDA value, mocking the fault, suppressing the Xid, weakening the fault verdict, or claiming that the GPU is reusable after the fault.

Dependencies: none for implementation. This change may land independently, but the real fault remains excluded from ordinary parallel gate execution only after the dependent terminal-scheduling dot lands.

Pre-change production failure: the full Maki entry reached the current post-fault EDFT-CORRECT$ assertion and printed expected 3, got 4 after the deliberate Xid 31. The standalone file can pass, showing device state rather than candidate correctness is the variable.

Acceptance: on DGX Spark, bin/hb --load maki/eval/device-fault-test.f must prove a green known-good candidate before the annotation, then the real faulter maps to EVN-DEVICE-FAULT, then the parent prints the final report without another CUDA launch. Off-device execution must record the existing skip and check-load. Mutating child isolation so the fault kills the grader must prevent the final report; mutating the fault verdict or moving any CUDA launch after the faulter must fail the owning regression or structural review. Run the exact file, its eval owner slice where safe, typed-local diff, package diff, trust, host, filemap, and suite coverage gates.
