---
title: "Hold the JSON reader's strings in vectors in its last byte rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T11:49:34.517618+01:00"
---

Problem: src/lib/json/read.f STR-INNER ( ^cursor ^u8 n -- n ) (:1197; callers json/value.f, equal.f, copy.f) still takes a buffer and size; tools/lsp-defs.f BYTES is a DYNAMIC-BUFFER with a BYTES-U length, so STR-B holds each string before a copy; tools/lsp-source-index.f ID-PKG and ID-TAIL are DYNAMIC-BUFFER plus -U pairs. Acceptance: each is a str or a vector (READ-STR-APPEND decodes straight into lsp-defs's vector, STR-B gone); json-read and lsp-boundary pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-lspvec (lsp part).
