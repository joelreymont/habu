---
title: Keep native cache misses quiet
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T16:55:52.592273+03:00"
closed-at: "2026-10-19T17:10:53.419304+03:00"
close-reason: "Implemented b8b20474: default dependency misses are quiet; HABU_NATIVE_CACHE_DIAG=1 reports the existing reason on stderr. Independent Astra review clear. Unchanged hb-build-aot-image E2E passes exit0; traced retained builder keeps stdout empty and produces a runnable rooted application. Explicit import refusal remains unchanged."
---

The unchanged hb-build-aot-image E2E fails its normal rooted maker call because NUNIT-DEP:CHECK prints four dependency-miss lines on stdout despite successful compilation. Default native cache fallback must remain quiet; HABU_NATIVE_CACHE_DIAG=1 diagnostics belong on stderr. Fix REPORT-MISS at its existing diagnostic boundary, preserve cache rejection and explicit count output, and verify the existing AOT image E2E plus diagnostic behavior with independent review.
