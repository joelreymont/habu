---
title: Fix uncached replacement checker storage refusal
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T15:49:12.327167+03:00"
---

Cold native builds exposed E-CHECKER-LAYOUT-BUFFER 7121 at the early SOURCE-UNIT typed declaration. The responsible load-order repair is implemented in 089cdf3b09c96ae7aaac7b7a0d5c2da698376616 and independently reviewed CLEAR: arm the type-declaration evaluator before the typed callback, load quotation storage before include, and drop the target continuation from the retired host heap before capture. A real cold product was built and passed source-unit-range, native-unit-key and native-unit-compile E2Es. The accepted worker workspace and temporary product were retired; source and tests remain in history. Integrated matching-engine, full native and convergence qualification remain open under habu-make-edited-native-59200861, so this dot stays active. Carl owns integration.
