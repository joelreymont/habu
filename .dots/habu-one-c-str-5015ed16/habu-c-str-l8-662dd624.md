---
title: "C-string L8: the lint owns PATHZ and PATH0"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T00:24:17.519112+02:00"
---

Problem: the engine's PATHZ and PATH0 (src/core/util.f) belong to src/core, src/habu and test/engine-suite.f, but nothing refuses a new caller elsewhere; test/aot-artifact-rows.f:54,73,89 is the one consumer using PATH0 today. Acceptance: tools/lint/bare-copy-lint.f reports whole-token PATHZ and PATH0 outside the owners with a finding naming FS:>PATHZ or FFI:CSTR; aot-artifact-rows.f opens through FS:>PATHZ; docs/forth.md 'Rules learned by refusal' gains the no-0-byte bullet (E-FFI-NUL through CSTR, E-PATH-NUL through the engine's copiers); the lint, its probe tree (one finding for tools/x.f, none for src/core/y.f), the artifact-row tests and check.f green. Files: tools/lint/bare-copy-lint.f, test/aot-artifact-rows.f, docs/forth.md. Verify: carl-dots/.out/carl-file/brief-l8.md proof list. Depends: habu-c-str-l2-4f11eee5. Ownership: those three files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-cstr-l8
