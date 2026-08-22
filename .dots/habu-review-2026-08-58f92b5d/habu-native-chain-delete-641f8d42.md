---
title: native chain delete candidates
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.916536+02:00"
---

Problem: measured zero/test-only consumers: hir-word.f:658-717,818-819,985-1000 DECLARE-UNMODELED/REASON@/AT/MODELED/DECLARE-OP/DECLARE-RENAME/DECLARE-CALLABLE (production resolves via RESOLVE-FIXED/RESOLVE-CALLABLE); emit.f:1800-1826 GOTO@/DROPPED/BLOCK-AT-POS@/INSN-MAX; abi.f NABI:LEAF/CALL/TAIL/TAIL-CALLING/POOL; judge-test.f:46-55 NAMED-CODE?/NAMED-REFUSALS (dead; REFUSED-ROWS covers it); migrate.f:248-251 E-NMIGRATE-ARITY arm unreachable per its own comment; test-only probes NINL:DECLINED, NSTR:COUNT/BYTES, NTRAP:COUNT, NPUB:OLD-*/NEW-*, A64RA:MOVES, NINP:RESUMED; NMIGRATE:DEFINE-HELD (tests 8, tools 0). Acceptance: each deleted or its consumer named in the commit. Files: src/compiler/native/, tools/judge-test.f. Verify: test/compiler suites. Depends: none. Ownership: native chain. Claim: unassigned.
