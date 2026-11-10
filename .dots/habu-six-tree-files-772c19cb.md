---
title: Six tree files are loaded by nothing
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:47:26.474134+02:00"
---

Problem: nothing in src, tools, test or docs loads or names src/habu/hide.f, src/habu/runtime-x64.f, src/habu/text-sites-x64.f, test/main-routing.f or test/aot-native-does.f, and src/habu/bundle-argv.f is named only by tools/lint/shadow-lint.f's file list (rg --fixed-strings of each basename on master feab8fff). Found by the n rule's census of unchecked files (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/ncensus2-part1.md). tools/wasm-build.f:70 also includes src/habu/prefix-rewind.f, which does not exist, so the tool fails at load. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt says for each file whether a build path or a test reaches it by a computed name; each unreached file is deleted or wired to the suite, and tools/wasm-build.f loads or is deleted with the Wasm build that replaces it (habu-emit-wasm-from-0d5dab47). Files: the seven named. Verify: the suite and the engine build after each deletion. Depends: none. Ownership: those files. Claim: unassigned.

2026-11-10 (bob): test/main-routing.f is wired to the suite by habu-docs-gate-md-3b1b9b21's commit; five files remain.
