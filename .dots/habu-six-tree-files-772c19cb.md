---
title: Three tree files are loaded by nothing
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:47:26.474134+02:00"
---

Problem: nothing in src, tools, test or docs loads src/habu/runtime-x64.f or src/habu/text-sites-x64.f (runtime-x64.f names only itself), and src/habu/bundle-argv.f is named only by tools/lint/shadow-lint.f:193's file list (rg --no-ignore --fixed-strings of each basename on a765f851). Found by the n rule's census of unchecked files. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt says for each file whether a build path or a test reaches it by a computed name; each unreached file is deleted or wired to the suite. Files: the three named. Verify: the suite and the engine build after each deletion. Depends: none. Ownership: those files. Claim: unassigned.
