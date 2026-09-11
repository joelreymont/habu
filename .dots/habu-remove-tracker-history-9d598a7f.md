---
title: Remove tracker history from source
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:40:19.213434+02:00"
---

Invariant: source comments explain stable code facts: the invariant, algorithm, ownership boundary, trusted assumption, error, or named documentation contract needed to maintain the implementation. They do not depend on temporary tracker identifiers, epic names, review authors, agents, phases, or unexplained plan labels. The current Forth tree contains at least 801 such comment lines across 340 files, plus unexplained labels such as B5.x; archived or deleted tracker state makes those comments decay into opaque history.

Classify every Forth comment that names a tracker, epic, subtask, person, review event, wave, or bare plan code. Replace it with ordinary English that preserves the durable technical reason; do not merely delete substantive constraints. A stable documentation reference may remain when it names the document and heading in human language. Move implementation history that is still valuable to version control or archival documentation. Remove personal attribution and unexplained phase labels from production and test comments.

Add a checked comment-token lint that rejects live tracker identifiers and personal review history in Forth source while excluding the dot ledger, strings, generated fixture payloads, externally specified protocol names, and archived documentation. Test multiline comments, strings containing tracker-like text, real identifiers, stable document headings, and malformed near-matches. Require zero findings, human review of every rewritten invariant, every touched exact source load, documentation checks, bootstrap, fixpoint, Maki, PTX standard library, and full native gates. Measure comment and source bytes only; emitted JIT, DATA, CODELEN, and behavior must remain identical.
