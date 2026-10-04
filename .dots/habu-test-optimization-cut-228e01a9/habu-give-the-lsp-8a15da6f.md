---
title: Give the LSP a located diagnostic for a verifier stop
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:33:35.877319+03:00"
---

Found by lexloc part A (09299d10, dot 44279554): check.f now renders a located record for every pre-verifier stop (7187/7188/7189 and part B's codes) in every mode, but the LSP (tools/lsp*.f through CHECK:VERIFY-BYTES) gets no diagnostic packet for a stop: VERIFY-BYTES answers `refused` and exposes VERIFY-STOP, VERIFY-STOP-AT, VERIFY-STOP-SUBJECT? and VERIFY-STOPPED$, while the record is rendered only in check.f's process, and tools/check-verify-core.f deliberately loads neither the lints nor the all-errors core. The same holds today for any verifier throw. Acceptance: an editor buffer with a stop (`  create` at end of input, an open string, an open PRIM: row) gets one LSP diagnostic with the code, line and column the check.f record carries, without loading the all-errors core into the LSP (render from VERIFY-STOP-AT through the LINT-LEX token lookup, or move the lookup to a layer both share); an lsp-test case seen failing first. After 44279554 part B. Files: tools/lsp*.f, tools/check-verify-core.f, tools/lsp-test.f.
