---
title: Make engine-size sum install-class images exactly
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T20:54:47.165843+03:00"
---

Problem: tools/engine-size.f sums an install-class image (one carrying an aot/checker-sidecar section) 8 bytes short of the file size and exits 74 there (private-words lane, 2026-09-16); it is exact on native-runtime images, so every published budget number is from those. Acceptance: the section walk accounts for the sidecar's header or alignment so the sum equals the file size on both image classes, with a fixture in tools/engine-size-test.f (or the tool's existing test) that builds or ships one install-class image and checks the sum; exit code only on a real mismatch. Files: tools/engine-size.f, its test. Verify: tools/engine-size.f on a native-runtime engine and on a build-fixpoint --install engine, both exit 0. Depends: none. Ownership: tools/engine-size.f. Claim: alder; .jj-ws/alder-sidecar-size from b4efad25.

The implementation is already repaired in ancestor 127ca372: ENGINE-ROWS charges the sidecar count through FRAME-CELLS exactly once. Remaining work is the dedicated framing regression; current native and private install images both measure exactly.

Validation: unchanged current tool exits 0 on private native product (4,391,104 bytes, sidecar 0) and private install-route candidate (5,505,216 bytes, sidecar 4,135,600), exactly their file sizes. No install --force or shared writes. New engine-size-fixtures row measures the donor's full file, then walks its payload with and without a seven-byte sidecar and pins the added framing count, alignment, offsets and 36-byte ENGINE-ROWS delta. It grows only its private buffer, so no spare text padding is assumed, and accepts either donor shape. Passes tiers 0/1 and an install donor; restoring the duplicate eight-byte sidecar charge fails assertion 12 by exactly eight. Independent Astra follow-up clear. Full gate remains Hazel's.
