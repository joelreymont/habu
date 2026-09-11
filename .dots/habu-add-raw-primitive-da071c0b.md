---
title: Add raw primitive row events
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:51.991951+02:00"
closed-at: "2026-07-26T08:59:33.254708+02:00"
close-reason: "Implemented, reviewed, merged: landed as c82a37cae0e2 (Emit primitive-axiom rows as one lexer token), an ancestor of master@origin. LINT-LEX emits one REGISTRY token per complete raw PRIM:/PPRIM: row with engine NEXT-RAW semantics for the eight quote-like labels; malformed rows set MALFORMED-REGISTRY with exact spans and stop the source. Delivery notes carried to the queue and follow-up dots: the slice-composition soundness finding, the non-short-circuiting bounds idiom, the LINT-WS? divergence, and the fail-open consumer wave remain tracked outside this dot."
---

Files: tools/lint/source-lex.f and tools/lint/text-foundation-test.f only, after LINT-LEX owns the boundary. Emit one REGISTRY token spanning each complete raw PRIM: or PPRIM: row. In normal lexical state only, recognize openers case-insensitively; consume the required name/header fields and every whitespace-delimited raw field through the matching PRIM; or PPRIM; with engine NEXT-RAW semantics, so `s"`, `c"`, `."`, `s\\"`, `c\\"`, `.\\"`, `[']`, and `[char]` are data. Comments and strings containing opener text stay inert. Missing headers, end of input, nested openers, and mismatched closers set MALFORMED-REGISTRY with the opener's byte, line, and column; SOURCE stops and exposes no later token. Acceptance: fixtures cover all eight quote-like labels in both row families, every malformed form, a real definition immediately after a closed row, fake openers and closers in comments and strings, exact diagnostic spans, and reuse after failure; a mutation disabling REGISTRY events fails. Verify: text-foundation lexer tests plus source-lex consumer focused gates, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=claude-solo workspace=.jj-ws/habu-add-raw-primitive-da071c0b.
Recorded retroactively at closure; implemented during the solo-orchestrator
shift.
