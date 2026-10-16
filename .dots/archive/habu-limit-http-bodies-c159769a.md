---
title: Limit HTTP bodies per route
status: closed
priority: 1
issue-type: task
created-at: "2026-10-15T17:22:20.202077+03:00"
closed-at: "2026-10-16T09:56:17.655579+03:00"
close-reason: ROUTE-CAPPED selects the route without captures and enforces its cap before buffering fixed or streamed bodies; captures bind after acceptance. Astra review and follow-up clear; focused HTTP suite passed on the selected qualified engine, including headers-only long-segment 413.
---

Phil supplied a public Habu reproducer: global BODY-CAP64MiB allows POST /chat to buffer32MiB before its handler, although only POST /uploads needs64MiB. Add explicit route body cap at route registration, enforced after validated head routing and before body buffering for Content-Length and incrementally for chunked input, with existing413 body_too_large response. Existing ROUTE inherits BODY-CAP as default; ROUTE-CAPPED overrides it with a nonnegative byte cap, zero permits empty bodies. Existing route matching and handler identity must be established once for cap and dispatch. Preserve default/missing-route behavior, keepalive and request cleanup, no application workaround or global mutable per-request cap. Use existing HTTP real-server E2E, one shared server with bounded case data including declared and chunked over-limit refusal and upload success. Phil repro: ~/.cache/tmp/kaizen-body-cap-repro/body-cap-repro.sh. Source-only feature, separate from core compiler batch; independent review and exact qualified public source release before caller adoption.
