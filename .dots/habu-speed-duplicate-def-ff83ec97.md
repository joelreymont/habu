---
title: Speed duplicate-definition lint on large generated sources
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T00:35:31.429508+02:00"
---

tools/duplicate-definition-lint-core.f currently passes focused fixtures but a full generated stage2-src scan exceeded 40s on Linux/aarch64 and was interrupted. Root cause appears to be whole-source vector lexing cost, not duplicate lookup after hashing. Fix with a streaming checked lexer or package-aware source scanner before wiring this lint into large build gates; prove on HB_TMP stage2-src under a small wall-time bound.
