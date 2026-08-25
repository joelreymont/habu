---
title: Root-cause text-foundation assert 424
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.921344+02:00"
---

Full context: tools/lint/text-foundation-test.f (stdlib/lint-artifacts/fast slice) dies at dynamic assertion 424 with no label (harness at line 26-30 prints only the counter before 1 die), identical on proofs parent 960bf2d5 and the seal merge. The index cannot be mapped statically because ASSERT is counted through loops; first step is a tool, not a bisect: extend the harness to print the failing section label (the file is sectioned) or break on ASSERT's false branch, then root-cause which tools/lint/text.f / token.f / source-lex.f behavior changed on the proofs branch. Note: the parallel paren-word lexer fix (dot habu-fix-paren-word-defe67e4) touches source-lex.f comment lexing — re-test after it lands; it may be the same root. Acceptance: the slice green through bin/hb --load on its exact gate composition, plus the harness upgrade so any future failure names its section.
