---
title: "Guard an executed word's arity and result type"
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T12:31:02.666678+02:00"
---

src/compiler/native/dict.f RUN-WORD is a TRUSTED boundary around one 'execute' of a word the engine's dictionary answered with. The checker cannot certify it: what a word entered through 'execute' consumes and leaves is not known where the call is written, so there is no effect to check the call against. NDICT:FIXED-VALUE measures the ARITY half dynamically - it records the data-stack depth before the entry and refuses any word that did not leave exactly one value - and that is all a count can settle. The TYPE half is unreachable today: a word that consumed one value and left two answers the count and still answers with the wrong number, and a created word's address ('ptr a') cannot be told from a constant's plain cell. WANTED: an arity-guarded execute with a typed result row - a form where the caller states the effect the entered word is required to have, the checker certifies the call site against that effect, and the engine refuses an entry whose word does not carry it. First consumer: NDICT:RUN-WORD, whose trust and whose depth count both retire when it lands. Sibling: the same shape src/compiler/ir/context.f CE-RUN keeps around its own execute.
