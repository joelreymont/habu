---
title: Suggest a require path the source can hold
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T16:48:24.701356+01:00"
---

Problem: REQUIRES:UNREQUIRED (src/core/requires.f) prints the target's display name, made relative to the root of the target's own load, as the text another file should hold. It is an absolute path when the target is not below that root (language server subjects, `require ../lib/e.f`), and a wrong one when the roots differ: from cwd q, `--load w/e3.f` tells lib/m3.f to add `require q3.f`, which gives E-MISSING-SOURCE. Acceptance: the suggested require loads when added to the named file, and the other names agree with the diagnostic's own position line; the two layouts and a language server case in the suites fail without it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rq-lsp.
