---
title: Use non-reserved PRODUCT parameter names
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T23:34:12.595349+02:00"
---

Problem: generated PRODUCT constructor/destructor effects choose positional variable names that reach reserved scalar spellings; at arity six the generated MAKE row uses f, which the checker parses as bool. A valid family such as span<q,u8,e,unique,transient,g> is expressible directly, but its generated constructor renders the last parameter as bool, blocking sound common types and making generator behavior depend on arity. Fix: make one generator-owned parameter-name allocator for every supported PRODUCT arity that emits only legal non-reserved type-variable tokens, and use it consistently for family declarations, MAKE, UNMAKE, field accessors, reflection, diagnostics, replay, and hashes. Do not special-case memory or add TRUSTED constructors. Acceptance: generated products at arities 1 through the supported maximum certify; every parameter remains independently polymorphic; constructor/destructor and field round trips preserve exact parameter position; reserved scalar tokens never appear as generated variables; repeated generation is deterministic; generic, linear, snapshot, AOT, bootstrap, and fixpoint tests pass. Add a red-first arity-six fixture matching the MEM span shape plus maximum-arity and wrong-position negatives. Files: the existing PRODUCT generator owner in src/core/type-family.f and its focused suites only. Verify: type-family/type-declaration/linear suites, generated-effect inspection, typed-local/trust/package/host/filemap/dot lints, fixpoint and full native gate.
