---
title: Check a file that sets its own source root
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:45:15.558017+02:00"
---

Problem: tools/check.f answers 'check.f: no such source' (rc 66) for Zen's src/server/server.f, which loads with rc 0 under hb --load from Zen's root: the file sets its root at its top level (SOURCE-ROOT:ACTIVE-SOURCE ... SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME SOURCE-ROOT:ROOT!) and then requires ../src/zen.f etc. relative to that root; check.f's closure walk (tools/check-core.f) resolves them without it. Eleven more Zen files (src/document/picture.f, edit-office.f, pdf-fonts.f, pdf-text.f, xlsx-values.f, src/server/credentials.f, mcp.f, test/document-publish.f, test/support/answer.f, job-complete.f) fail the same way. Acceptance: check.f on each such file run from its repo root gives the result hb --load gives (rc 0 for server.f) and checks the files it requires. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-root (root-cause hunt).
