---
title: Package bootstrap mirror lint core
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:51.772904+02:00"
blocks:
  - habu-pkg-bootstrap-mirror-4021501a
---

Why: tools/bootstrap-mirror-lint.f still publishes 27 package-less words plus 13 package-less constants, buffers, and cells, while its landed white-box test already reopens the intended owner. A representative private-tail edit fails package-diff-lint with E-PACKAGE-OWNERSHIP.

Owner and files: package BOOTSTRAP-MIRROR-LINT; tools/bootstrap-mirror-lint.f and tools/bootstrap-mirror-lint-test.f only. Publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), FINISH ( -- ), and RUN ( -- ). Keep every constant, buffer, cell, scanner, predicate, renderer, walker, and source loader private. Remove BML- from every private name; use SRC-A, SRC-U, SRC-CAP, FILE-A, FILE-U, BAD-N, FILE-N, SCAN-I, and NUM-I for state. Reopen BOOTSTRAP-MIRROR-LINT in the test, update white-box references to the private tails, rename its private RUN to TESTS, invoke TESTS before ;package, and expose no test API.

Preserve exactly: source-root walk, .f/.fs filtering, test and native-only exclusions, complete-source lexing, case-insensitive declaration keywords, definition-name and escaped-token exemptions, source labels, line numbers, finding text, counts, exit 1 on findings, and clean exit. Acceptance: zero executable BML-prefixed definition, storage, or reference remains; public RUN is the sole production walk entry; removing the package opener or leaving any former global fails the exact package gate; clean source, dirty overlay, clean overlay, comments, strings, definition names, escaped references, mixed case, and every declaration keyword execute the real production scanner.

Forbidden: compatibility aliases, forwarding globals, exported state, second scanner, copied lexer logic, substring-only structure checks, new requires, changed exclusions, or behavior changes.

Verify: bin/hb --load tools/bootstrap-mirror-lint-test.f; call BOOTSTRAP-MIRROR-LINT:RUN through the real source walk; exact-diff typed-local and package lints; hostile package-opener removal; host-lint; filemap-lint.

Claim: agent=bootstrap_mirror_core workspace=.jj-ws/habu-pkg-bootstrap-mirror-663e20b8.
