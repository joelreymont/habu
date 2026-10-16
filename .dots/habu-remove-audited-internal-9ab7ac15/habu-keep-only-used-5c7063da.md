---
title: Keep only used IR attribute kinds
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.611792+03:00"
---

ir/attr.f supports nine kinds but production constructs only int and symbol; payload pool, enum families, staging/rendering and builder wrappers for other kinds exist only for tests. Retain actual integer/symbol interning, ownership and consumers; delete unused APIs, storage and exclusive tests. Run existing IR attr/build and native/Wasm downstream tests; integrated matching compiler product/full native gate as required.
