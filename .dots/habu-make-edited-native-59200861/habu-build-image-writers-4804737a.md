---
title: Build image writers outside native capture
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:02:13.271708+03:00"
closed-at: "2026-10-20T22:36:52.609538+03:00"
close-reason: Native scopes retain target compilation, cleanup and owned capture; the uncaptured source writer uses the host tier afterward and restores it on throws. Entry, nested-scope refusal, writer recovery, bundle and supplied-writer E2Es pass; independent Astra review clear. Controlled whole-build CPU fell 33.67 to 10.74 seconds with exact executable and map bytes.
---

Whole-path CPU profiling shows a small early compiler edit spends25.369s rebuilding the post-capture image writer versus.951s for a late edit; main replay remains7.96-8.18s. End the driver-owned native scope only after target hooks, capture and OWN-WINDOW produce an owned value, then load the transient source writer at tier0 on AArch64 with ordinary tier restoration (x64 remainsnative). Preserve native provenance refusals, fresh target layout/compiler-role keys, suppliedwriters, source-input recording, same-machine restrictions and throw cleanup. No scope-bypass primitive, donorwriter reuse or permissivefallback. Measure early/late and cold/cached image-map equality, existing tierrefusals, suppliedwriter/bundle and errorpaths; independent review before landing.
