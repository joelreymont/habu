---
title: Package shared lint image
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:29:47.567224+02:00"
---

Frozen revision f7ed6085 loads an ambient 2,166-word pseudo-namespace into the shared standard-library/lint image (test/gate-stdlib-inline-lib.f:175-201,358-375). Census: tools/date.f 77 globals; lint text/source-lex/intern/token/lib/json-writer 291; tools/json.f 258; ten shared *-core files 991; seven lint cores 549. Generic names such as READ-FILE, SP?, TOK, TOKENIZE, INTERN and SIG-KIND collide globally, while 251 names exceed the 16-byte inline-name limit and therefore add concrete extended dictionary-name storage. Migrate by owner: DATE, JSON-READ, LINT-TOKEN, LINT-LEX, LINT-INTERN, LINT-PATTERN, LINT-JSON-WRITER, a small LINT facade, and one package per lint core. Export only lifecycle/run/result APIs; state, buffers, cursors, classifiers, scanners, and rendering helpers stay private; tests reopen owners for white-box checks. No legacy prefix aliases. This is a controller: decompose by non-overlapping owner/file groups before dispatch. Acceptance: each component standalone-loads; shared-base/lint slices retain exact output, diagnostics, counts, and exits; old prefixed/generic globals and qualified private helpers reject; public APIs certify; long-name, dictionary, JIT/DATA/CODELEN measurements shrink without throughput regression; focused lint tests, shared gate, package/host/dot and full native gates pass. Excludes hb-build package work already owned elsewhere.

Currentness addendum: new tools/lint/diff-error.f is a one-constant package-less module exporting global E-DIFF-SYNTAX even though its sole owner/parser is package DIFF. Reopen DIFF in the error module, expose short qualified E-SYNTAX (or the repository-standard qualified error tail), update framed/parser consumers, and remove the global alias. Prove bare and wrong-package access reject while exact diagnostics/codes stay unchanged.
