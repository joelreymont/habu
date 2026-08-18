---
title: "Definer and evaluate: 104 sites, capability for twelve"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:44.313164+02:00"
---

Phase 5 of 4fd12d60, class B: 104 metaprogramming sites, but 84 are the bare evaluate boundary (TRUSTED: EV ( ptr u8 n -- ) evaluate ;) and only ~12 in the decl machinery need a true definer-typing capability (create/does>, is-installs, ctor generation). Design question to probe FIRST: whether evaluate can carry a checked contract at all (the evaluated text's effect is unknowable statically) - the honest outcome may be ONE sealed engine primitive for evaluate (like the code-emission class) plus definer typing for the 12. Do not mint an evaluate axiom that pretends to know the text's effect. Blocks the final deletion.
