---
title: "Maxima integrate path: resolve unbound symbols"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-02-17T15:22:13.051560+01:00\\\\\\\"\\\"\""
---

After subset load (41 files),  returns UnboundSymbol. Identify missing module/dependency chain and compile/runtime gaps; make integrate(x^2,x) succeed. Depends on Maxima loader server coerce crash and habu-fix-maxima-cas-a491af14
