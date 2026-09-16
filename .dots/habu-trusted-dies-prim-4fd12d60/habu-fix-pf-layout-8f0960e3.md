---
title: Fix PF-LAYOUT-REQUIRE frame ordering in optimizing selfbuild
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T17:19:33.198324+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.782892+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The optimizing self-build still stops with an uncaught -8522 frame-ordering failure compiling PF-LAYOUT-REQUIRE"
---

Owner requested: Rowan independent compiler lane; cedar integrates and owns full selfbuild. Source c0bd71d4 includes all reviewed KEEP, lexical quotation, trap-join and allocator fixes. Matched bootstrap checkpoint /tmp/cedar-current-layout-checkpoint-2 SHA2563e40801b9e492da4af82f8e80f414a00fdef7be85220e7e4514f821e4f60dccf. Actual optimizing selfbuild `HABU_TARGET=linux-aarch64 <checkpoint> --load test/compiler/aot-mode.f tools/native-build.f` in cedar-aot-selfbuild passes prior ACAP-TIDX-INS failure then stops rc67 after203.854s: ncomp cannot compile PF-LAYOUT-REQUIRE, driver uncaught -8522 (frame ordering). Log /tmp/cedar-current-optimizing-selfbuild-2.log. No optimizing output produced. Acceptance: reduce valid function to fast load-path reproducer, identify responsible selection/spill/verifier layer, preserve real frame-order negatives, add regression and independently review, then rerun sole full selfbuild. Combined explicit-tier1 rstack, quotation and regalloc suites and real CLI/argv tests pass on checkpoint.
