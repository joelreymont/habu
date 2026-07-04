---
title: "Core: structures DSL syntax + effects-file review"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T17:59:14.022012+02:00"
---

User FIXMEs in src/core/structures.f:17,:39 and structures-effects.f:2 (main worktree; files currently being modified by the TFAM campaign - coordinate before touching). (1) Syntax question: today the pair is BEGIN-STRUCTURE ... END-STRUCTURE (ANS Forth standard names). User asks why not STRUCTURE ... ;STRUCTURE. Answer: BEGIN-/END-STRUCTURE is the ANS-standardized pair, and repo convention keeps core/built-in words as-is (CLAUDE.md: built-ins stay lower/standard case; project words are the UPPER-CASE ones). A ;STRUCTURE alias would add a second spelling for one concept and ;-prefix connotes ending a definition, not a scope - same reasoning as the declined ;PACKAGE. If a rename is still wanted it is a mechanical tree-wide task - decide, do not drift. (2) structures-effects.f: it exists because the early structure defining words are TRUSTED with declared effects loaded separately during boot (checker cannot infer them at that stage). Whether it can be RETIRED depends on the TFAM product-family work (typed records may replace the early +FIELD machinery entirely - see habu-epic-type-habu + TFAM 15). Task: when TFAM 15 lands, evaluate folding/deleting structures-effects.f and de-TRUSTing the survivors; until then the file stays. Owner: TFAM campaign; this dot exists so the question does not fall through.

DECISION 2026-07-04 (user): RENAME APPROVED, and generalized into a repo
convention - scope pairs are FOO terminated by ;FOO. For this dot:
BEGIN-STRUCTURE ... END-STRUCTURE becomes STRUCTURE ... ;STRUCTURE (closer
mirrors the opener's case with a ; prefix). Scope: src/core/structures.f
definitions, structures-effects.f TRUST rows, every use site tree-wide,
TRUSTED.md pinned rows, docs/forth.md examples. SEQUENCING: structures.f and
structures-effects.f are dirty in the TFAM campaign's tree - execute this
rename immediately AFTER their current work lands (or hand it to that
session); a fable-side rename now would conflict across every TFAM commit.
Gate: full engine refresh + test/run.f + maki (structures is fixpoint input).
