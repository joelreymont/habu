---
title: Remove legacy global fallback lookup
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-24T19:17:45.160470+01:00\\\"\""
closed-at: "2026-02-24T19:19:43.371093+01:00"
close-reason: Drop legacy global fallback name probes
---

src/interp/vm.zig lookupSymbolGlobalIndex: remove legacy CL/CL-USER prefix fallback probing and enforce qualified-name lookup only. Add regression test proving unqualified global name does not resolve implicitly. Validate with focused vm cache tests and maxima-hotspots baseline.
