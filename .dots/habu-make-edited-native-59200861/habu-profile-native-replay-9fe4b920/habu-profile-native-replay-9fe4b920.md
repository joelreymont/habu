---
title: Profile native replay after rewind
status: closed
priority: 2
issue-type: task
created-at: "2026-10-17T00:32:54.953074+03:00"
closed-at: "2026-10-21T13:29:22.837355+03:00"
close-reason: ARM64 trim/code-reuse boundaries synchronize pending samples before retiring searchable spans; historical counts and caller edges remain reportable. Pre-code exclusive wrong-owner regression, alias/reclaim/index/reset and native gate pass; independent Astra review clear. Real profiled and unprofiled native products/maps are byte-identical. Conservative stack-derived inclusive-only false callers remain a profiling limit; exclusive PC ownership is corrected. Final native debug row9.218s and five identical engines/maps qualify the change.
---

Existing build-profile indexes donor code before native-build rewinds/reuses its code addresses. A real warm run preserved12989definitionhits/916compiles but reports947samples as TDECL-SUMV-PROVIDER where final target PC is TDGEN-OUT-TYPE. Correct existing profiler/build lifecycle so post-rewind samples resolve actual target words. Obtain trustworthy fallback names/reasons at existing definition-cache dispatch before choosing edit-speed optimization; avoid general tracing framework. Preserve ordinary product bytes, disabled-profile behavior and existing native/profile boundaries; focused real build artifact validates attribution.
