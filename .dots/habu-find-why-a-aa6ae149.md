---
title: Find why a --repl image ignores --load
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:46:16.760574+02:00"
---

Problem: a program built with tools/hb-build.f --repl and run as `<image> --load FILE` printed nothing and exited 0, while FILE on standard input ran (/tmp/claude-501/seal/hb-sealed). A flag dropped without a word hides a failure. Acceptance: the cause as path:line with evidence: what such an image does with its arguments, and whether --load is meant to work there; the fix is a second brief. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/hunt-repl.
