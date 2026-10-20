---
title: Keep callback scopes bootstrap-compatible
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T09:57:24.347877+03:00"
---

Reviewed SOURCE-INPUT:WITH typed callback cells stop a real native build at 4931 compiled definitions with E-CHECKER-LAYOUT-BUFFER 7121. Frozen source9305794e, hb-core-batch08e1 host, external callback-cache; logs callback-build.out/err under habu-native-definition-batch.3tinjw2e. Reduce the rejected declaration/owner metadata and fix the responsible bootstrap cell/callable boundary without weakening layout admission or source-callback restoration. Existing maker/input E2Es need the rebuilt scope API; acceptance is actual cold prefix and matching engine build, focused scope E2Es, Astra review.
