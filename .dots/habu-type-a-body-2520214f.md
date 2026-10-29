---
title: Type a body char as its character literal in the checker
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T03:48:01.351556+02:00"
---

Workspace: jerry-chk. Lands inside the T0 stack (commit nspsunws, on the char and signature commits), not alone on master: master's JIT compiles a body `char` by dictionary lookup, so the checker change alone would certify `: char ( -- n n ) 1 2 ; : W ( ptr u8 -- ptr u8 ) char x drop ;` (forged ptr, SIGSEGV); T0 compiles it as the literal. A definition body's char/[char] with its operand is typed as the character literal (pushes n, operand swallowed) whatever dictionary word shares the spelling, as capture and tier 1 compile it. Today DO-TOK types the token by a user word named char, then LITERAL-PAYLOAD-STEP swallows the operand: ': char ( n -- n ) 1+ ; : U4 ( n -- n ) char x ;' is accepted (tier 1 -8303, T0 prints 120), and ': char ( -- ptr u8 ) s" ab" drop ; : U8 ( -- ptr u8 ) char x ;' forges a ptr. Fix at src/core/checker.f DO-TOK1 / LITERAL-PAYLOAD-STEP / PARSE-LIT?; regression rows in the checker suite holding body-literal rules.
