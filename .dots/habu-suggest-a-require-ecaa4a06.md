---
title: Suggest a require path the source can hold
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T16:48:24.701356+01:00"
---

Problem: in the language server E-UNREQUIRED names the missing file by an absolute path, for example 'add require /tmp/ws/workspace/w/d.f to z.f', so following its help writes a machine path into a source (tools/lsp-test-lib.f references-* and rename-* cases show it). Acceptance: the help and the expected note spell the path as the file's own require would, relative to the source root or the requiring file, and a case pins it. Depends: habu-reject-a-file-3120d5e1. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rq-lsp.
