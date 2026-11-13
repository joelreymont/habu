---
title: Six tree files are loaded by nothing
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:47:26.474134+02:00"
---

Problem: nothing in src, tools, test or docs loads or names src/habu/hide.f, src/habu/runtime-x64.f, src/habu/text-sites-x64.f or test/main-routing.f, and src/habu/bundldoes.f, and src/habu/bundle-argv.f is named only by tools/lint/shadow-lint.f's file list (rg --fixed-strings of each basename on master feab8fff). Found by the n rule's census of unchecked files. tools/wasm-build.f:70 also includes src/habu/prefix-rewind.f, which does not exist, so the tool fails at load. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt says for each file whether a build path or a test reaches it by a computed name; each unreached file is deleted or wired to the suite, and tools/wasm-build.f loads or is deleted with the Wasm build that replaces it (habu-emit-wasm-from-0d5dab47). Files: the six named. Verify: the suite and the engine build after each deletion. Depends: none. Ownership: those files. Claim: unassigned.

2026-11-10 (bob): test/main-routing.f is wired to the suite by habu-docs-gate-md-3b1b9b21's commit; five files remain.

2026-11-10 (bob), checked on master 421bb647: tools/wasm-build.f is gone and its missing include with it. src/habu/hide.f, src/habu/runtime-x64.f and src/habu/text-sites-x64.f are still namnative-does.f are still named by nothing; src/habu/bundle-argv.f only by tools/lint/shadow-lint.f:197. These four remain.
2026-11-12 (jerry): src/habu/hide.f is deleted. Its only loaders, tools/bootstrap.sh (which appended it) and tools/bootstrap-codegen-test.f (which included it), were deleted with the old builders in 1d97c3f2, and `rg --no-ignore 'hide\.f'` over src, tools, test and docs finds no other name. The four others remain.
