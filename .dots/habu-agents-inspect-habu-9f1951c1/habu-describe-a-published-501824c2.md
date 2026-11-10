---
title: Describe a published word in the live REPL
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T19:42:25.701245+02:00"
blocks:
  - habu-design-the-checker-b77c228b
  - habu-measure-what-an-5396ae6b
---

Problem: an agent in a Habu session learns a word's binding, declared effect and source only by reading the implementation or guessing; the facts exist (a definition's facts on its dictionary entry; the checker's effect query, src/core/checker.f:15069 EFFECT-QUERY and its EFFECT-DIN-*/DOUT-* readers; src/habu/xref.f), but no REPL word answers from them: SEE and XREF (src/habu/xref.f:791-795) print a word's raw record (name, start, length, flags, wordlist), and tools/lsp-hover.f:34-46 renders kind, effect, package and file:line for an editor, not for the REPL.
Acceptance: one public word takes a name in the caller's actual scope and returns its resolved qualified identity, canonical declaration and effect, kind and call authority, and source location when the definition was loaded in this session; absence and ambiguity are reported explicitly; an engine-held word reports no source rather than inventing one; a definition loaded in the session is distinguished from edited source not yet loaded; a readable effect alone never claims the caller may call the word. The answer is owned data or a copy into a caller-owned buffer, built from the existing records and renderers; no JSON or request shape inside the REPL call. Which EFFECT-* readers are public after sealing is established first; the word exposes the checked operation, not the storage. Documented in docs/forth-card.md with one copyable REPL interaction. Joel, 2026-11-10: the word answers from the source index as well as the live dictionary: a word defined in the Habu source but not loaded, or not visible where it is asked about, is reported with its defining file and the reason it is not visible, never as absent.
Files: a new module under src/habu/ (name by convention), src/core/checker.f only if a reader must be made public, docs/forth-card.md, docs/forth.md.
Verify: in one bin/hb session: define a word, describe it, and read its identity, effect and location without restarting; describe an unknown name and an engine-held word; the two answers differ as stated.
Depends: habu-design-the-checker-b77c228b (the facts this word reads); habu-measure-what-an-5396ae6b (the measurement, which leads the campaign).
Ownership: the describe module and its docs. Claim: unassigned.
