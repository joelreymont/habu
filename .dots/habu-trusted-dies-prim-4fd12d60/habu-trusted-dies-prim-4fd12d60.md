---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

Goal: ordinary Habu is checked code; a genuine engine, syscall or FFI boundary
is a primitive effect its package owns; the TRUSTED: definer is removed
(habu-delete-the-trusted-42b30edd).

Problem: TRUSTED: definitions remain on master 58f801e1: src 30
(src/lib/xml/c2.f 8, src/habu/verify-source.f 6, src/lib/c2-memory.f 5,
src/core/cell-effects.f 3, src/habu/interpreter.f 3, src/core/check-walk.f 2,
src/core/checker.f 1, src/core/check-hook.f 1, src/compiler/native/dict.f 1),
tools 0, test 23 (the four evaluate wrappers are habu-give-tests-a-8d8cdc19).
interpreter.f's DEF-TRUST-SIG, DEF-PREFIX-BIND and DEF-XT! are the definer's
own writers and go with 42b30edd. src/lib/xml/c2.f's 8 are rejected in every
checked form (E-CAST-LINEAR, E-LINEAR-OWNER, E-CAST-SCOPE, exclusive copy,
E-CAP-TRUSTED): a checked form needs a public word that would let any package
forge an XML reader or alias an exclusive view.

Acceptance: each remaining body is checked code, or a primitive owned by its
package where the operation is genuinely unmodelled; `rg -n '^\s*TRUSTED:'
src tools test` finds nothing; 42b30edd's acceptance holds.

Depends: habu-give-tests-a-8d8cdc19, habu-delete-the-trusted-42b30edd.
Claim: unassigned.
