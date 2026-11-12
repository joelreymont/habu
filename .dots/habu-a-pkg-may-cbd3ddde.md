---
title: A package may publish a word named ;
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.557659+02:00"
---

Problem: policy-admit admits a package whose public wordlist holds a word spelled `;`. POLICY-KEYWORD (src/habu/interpreter.f:1502) walks the package's wordlist but tests each name against six hand-kept spelling tables (DEF-KEYWORDS, DEF-DESIGN-KEYWORDS, DEF-INTERPRET-KEYWORDS, DEF-NAME-ONLY-KEYWORDS, DEF-OP-KEYWORDS, DEF-P2-KEYWORDS, :1447-1471, through DEF-IN-TABLE? :1474), because the interpreter reads its constructs by `TOKEN-IS?` string compares and no wordlist holds them. Nothing ties those tables to the compares. Joel, 2026-11-11, told that the row walk answered "is this name a keyword?" from rows of keyword spellings: "that's stupid, this should have walked the wordlist instead". docs/compilation.md:26 "One reader" says the same: every construct is one immediate word in the dictionary, "there are no keyword lists".

Acceptance: the constructs are immediate words in a wordlist and the interpreter finds them by the lookup; a package publishes a keyword exactly when a name in its public wordlist is found among them, so `;` is refused; the six tables, DEF-IN-TABLE? and its callers are gone; src/lib/vocabulary-test.f has refusal cases for `;` and one spelling from each former table. Depends: habu-read-each-def-f7ce769a (this is part of the one-reading design); the row walk (habu-delete-the-jit-d9fdaf67) landing first. Claim: unassigned.
