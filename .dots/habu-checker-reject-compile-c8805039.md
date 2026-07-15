---
title: "Checker: reject compile-time file loaders"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T18:32:53.381249+02:00\""
---

Full context: a checked definition containing parse-name include/require or stack-string included/required/provided is accepted, but the native compiler executes the immediate while the dictionary is RX; nested evaluate then reaches LCEMIT with CP in the RX dictionary and faults at str w9,[x28] (direct reproducer: : MAIN ( -- ) 73 include /tmp/habu-fragment.f . ;, LLDB pc bin/hb+0x14098, x28=0x3001854ac). Make the static boundary fail closed before runtime: classify file-loader parsing immediates as top-level-only, reject every loader token in a checked body with an exact checker diagnostic, preserve top-level loader behavior, and ensure the native compiler cannot execute this crash path even under unchecked input. Add a minimal negative checked regression for all five loader forms on the exact load path, a direct compiler negative regression, fail-closed rc/diagnostic proof, bootstrap/fixpoint parity, and document the top-level-only invariant. Do not add runtime guards or library workarounds; no SUMTYPE/PRODUCT syntax.
