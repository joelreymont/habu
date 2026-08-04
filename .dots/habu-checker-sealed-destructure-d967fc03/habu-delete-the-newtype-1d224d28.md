---
title: Replace NEWTYPE with carrier form; delete DEFTYPE
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T15:42:15.774373+02:00"
---

Why (Joel, 2026-07-30): the type-declaration surface collapses to records,
alternatives, and ONE real newtype. Today's NEWTYPE grammar (name + arity)
never states what the wrapped value is; DEFTYPE states nothing either, is
hardwired to n (a nominal float is inexpressible), and duplicates a 1-field
structure with friendlier converter names. Both are replaced by one
Haskell-style declaration with a STATED carrier.

Result: NEWTYPE takes name + carrier type — `NEWTYPE idx n`, `NEWTYPE eps r` —
and is checker-sugar for a 1-field STRUCTURE over that carrier: one cell at
runtime, a distinct nominal at check time, deriving the visible converter pair
(`>IDX ( n -- idx )`, `IDX>N ( idx -- n )`; converter tail spelled from the
carrier). `CONSTRUCT owner` composes exactly as on any structure: flagged
newtypes derive no public constructor and the owning package validates. The
old arity grammar is deleted (E-UNDEFINED through the production load path);
DEFTYPE is deleted after every declaration migrates. Migration census
(2026-07-30): 118 arity-0 NEWTYPEs (most evaporate under CONSTRUCT owner on
their real carrier structures; the rest become carrier form), 44 parameterized
NEWTYPEs (carrier form with type parameters — the contract freeze decides the
parameter syntax before dispatch), plus every DEFTYPE (all package-scoped
1-cell n wrappers; mechanical).

Dependencies: the CONSTRUCT owner flag leaves of this campaign land first.
Production red: `NEWTYPE idx n` fails to parse on the current tree and DEFTYPE
still resolves. Acceptance: carrier-form declarations parse and check; the
nominal is one cell and rejects its bare carrier both directions (probed);
a float carrier works; old arity grammar and DEFTYPE both fail E-UNDEFINED
through the production load path; every migrated consumer suite green; native
fixpoint passes. Forbidden: compatibility alias, retained old grammar,
transparent (unchecked) alias mechanism, second converter-naming scheme.
