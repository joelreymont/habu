---
title: Unicode math spellings in the equation grammar
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T14:34:54.836011+02:00"
---

Joel 2026-07-19, reversing the ASCII-only decision recorded in maki/spec.f's header: the equation surface accepts the real math spellings. Design (the confusable-normalization fix, not the timid ASCII retreat): the equation lexer normalizes the small confusable set so identical-looking codepoints are ONE token - GREEK CAPITAL SIGMA U+03A3 and N-ARY SUMMATION U+2211 both lex to the summation keyword (alias of +SUM); MIDDLE DOT U+00B7 and DOT OPERATOR U+22C5 both lex to the product keyword (alias of *). ASCII spellings stay legal forever (terminals, greps, diffs). Grammar shape: 'Sigma k' prefix form as shown in the pitch (summation token followed by the contraction index list) accepted alongside the trailing '+SUM k'; decide and document ONE canonical pretty form in golden-syntax.md. Any OTHER non-ASCII byte in an equation is a named E-SPEC-SYNTAX reject with a diagnostic naming the offending codepoint - no silent acceptance of lookalike soup (that silent-confusion class is why ASCII-only was chosen; normalization kills it properly). Tests: both codepoints of each pair parse identically; a stray other Unicode char rejects with the named diagnostic; the pitch's exact line works as written. Serialize behind the equation stage-1 lane (same file, maki/spec.f).
