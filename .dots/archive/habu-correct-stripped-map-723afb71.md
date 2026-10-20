---
title: Correct stripped map fixture expectations
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T13:56:06.695993+03:00"
closed-at: "2026-10-16T14:05:30.264694+03:00"
close-reason: "Removed incidental anonymous/baked-code requirements while preserving full attribution coverage, real baked-source checks and exact runtime output. Complete stripped-cache E2E passes with reviewed object-pair publication: exit 0, test: ok, 162.13s wall / 89.97s user / 4.06s system on private 7cc7f9be donor; no final engine qualification claimed."
---

The stripped-cache E2E fails three incidental map assumptions on the current matching donor. maplate retains no separate anonymous loaded word, while ptrcell carries STR-MAX-I64$ DATA and can require no baked code. Its digest-bound map still covers all code and the executable prints the expected bytes. Remove only the anonymous-row requirement and scope baked-source assertions to fixtures that actually retain baked code. Preserve total code coverage, loaded/generated attribution, donor-map-key invalidation, object-cache relinking and exact runtime output; run the complete stripped-cache E2E with the reviewed publication fix.
