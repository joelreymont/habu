---
title: Decide what a local named after a dialect word means
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T13:20:42.095575+02:00"
---

src/compiler/native/elaborate.f DECLARE-LOCAL refuses a {: ... :} local whose bare name the word model already models - '{: i:n :}' inside a counted loop would otherwise make 'i' mean two things, and which one it means is a rule the elaborator has no business inventing. The refusal is E-NELAB-LOCAL and test/compiler/native-elaborate.f SHADOW-CASE pins it. Wanted: establish from the ENGINE what a Habu local named after a built-in really does (compile a definition that declares one and read what it computes), then either make the elaborator agree with that or keep the refusal and say in docs/forth.md that such a name is not allowed. Do not guess.

MEASURED 2026-08-09: 47 definitions, every one a local named `i`. Engine datum:
`: DBL ( n -- n ) 2 * ; : SH ( n -- n ) DBL 1 + {: DBL:n :} DBL DBL + ;`
answers 22 for 5 - a token resolves where it stands; the earlier mention is the
global, later ones the local. The chain now agrees on the general shadowing
shape (elaborate.f LIVE-AT?, pinned by the LGP-SHADOW case); this dot's open
question is only the loop-index case.
