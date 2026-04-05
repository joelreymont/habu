---
title: Fix rtest6 string constructors
status: active
priority: 1
issue-type: task
created-at: "\"2026-04-05T11:37:31.453076+02:00\""
blocks:
  - habu-split-load-apis-e4c62eb7
---

Problem: PLAN.md 2.1a0 is still undotted: canonical rtest6 now first hits the string/array constructor path around ../maxima/tests/rtest6.mac:94-108 before later operator failures. Acceptance: canonical tools/maxima-rtest.lisp rtest6 advances through the string/array constructor forms with correct CL semantics and no Maxima-specific patching. Files: PLAN.md:560-577, ../maxima/tests/rtest6.mac, src/runtime/primitives/string.zig, src/runtime/primitives/array.zig, src/compiler/compile.zig, src/interp/vm.zig. Verify: focused rtest6 slice plus full canonical rtest6 progress past the constructor floor.
