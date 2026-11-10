---
title: "SOURCE-ROOT:WITH-CWD is public and nothing calls it"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:57:06.550300+02:00"
---

Problem: src/core/include.f:554 WITH-CWD is a public word of SOURCE-ROOT with no caller in src, tools, test or docs (rg -n WITH-CWD finds only its definition); it opens a root scope SCOPES counts, so it is a second way to do what WITH does. Found by the review of habu-src-root-cd-610b1c3d. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the word is gone, with any word only it calls; the engine builds and test/source-root-exe-test.f passes. Files: src/core/include.f. Verify: rg; the engine build; the test. Depends: none. Ownership: the source loader. Claim: agent=bob workspace=.jj-ws.noindex/bob-withcwd
