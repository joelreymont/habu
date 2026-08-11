---
title: CHECKER-DEFINED? answers in the open package scope - audit and fix the shape
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T01:12:30.000000+02:00"
---

Found by the fixpkg lane (2026-08-11), isolated probe on record: s" FS-PATH-CAP" CHECKER-DEFINED? answers no when asked from inside a package (the checker resolves through CHECKER-RECORD-SYM -> CHECKER-PKG-CONTEXT, so a bare global name is looked up as a private tail of the open package) and yes from outside - a load-discipline guard silently inverts when its file gains a package. Same class: anything running in-process certification (VERIFY:SOURCE-BUF) verifies against the open package wordlist. Two-part work: (1) audit every CHECKER-DEFINED? and in-process-certify call site that sits inside a package for silent inversion (rg for the callers, classify each); (2) answer the checker-miss question - what rule makes this bug impossible? Candidates: a scope-explicit query (global-scope variant or qualified-name resolution in CHECKER-DEFINED?), or a lint that refuses a bare-global CHECKER-DEFINED? inside a package. Fix the shape or dot the missing capability; do not leave per-site workarounds as the pattern. Files: src/core/checker.f, tools/package-diff-lint-core.f, callers found by the audit. Depends: none.
