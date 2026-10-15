---
title: Retain origin FILE owner callbacks
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T07:06:21.331849+03:00"
---

Actual warm FILE replay on donor4 found ENGINE-ORIGIN-EARLY:FILE-MARK missing from the captured dictionary while full FILE-ROWS remained. Without a valid lower bound, the old exporter produced an ABSENT-phase section containing 14296 historical rows. New codec must decline caching when the active observer marker is unavailable, and the matching native product must retain its dynamically resolved origin owner callbacks at the existing capture name boundary. Enumerate only actual dynamic names and exact packages; no package wildcard or new callback registry. Cover current mark/rows/state/hook/span/restore owner queries needed by the codec and source-only transition. Acceptance: fresh product exposes the required coherent owner callbacks; engine-origin.f, native-runtime.f and symbol.f demonstrate actual warm hits with correct public/private origins; existing malformed-section and native/convergence gates remain mandatory.
