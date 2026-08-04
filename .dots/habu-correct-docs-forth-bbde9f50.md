---
title: Correct docs forth guide on shipped STRUCTURE
status: open
priority: 2
issue-type: task
created-at: "2026-07-25T14:42:11.658847+02:00"
---

Why this is needed: docs/forth.md is the coding standard every session is required to read, and its type-declaration section states the opposite of what the engine ships. Lines 489 to 495 say that no unified STRUCTURE opener exists, that it is a planned replacement, that it is not yet implemented, and that loading a STRUCTURE declaration fails with E-UNDEFINED: STRUCTURE and exit 70. A newcomer following the standard would avoid the declaration form the project has already standardised on.

Measured on master 79c50e5a9dbf: src/core/structure-decl.f defines the word at line 328 as `: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;`, documents the grammar at line 11 as `STRUCTURE type-name arity header-clause* field* ;STRUCTURE`, and 21 tracked source files under src, lib, tools, test and maki already open STRUCTURE declarations. Loading a STRUCTURE declaration does not produce E-UNDEFINED; a malformed one is rejected by the declaration parser with throw 7108. One part of that paragraph is still accurate and must stay: E-REMOVED-TYPE-SYNTAX appears nowhere in the engine.

Owned result: rewrite that section of docs/forth.md so it describes the shipped STRUCTURE declaration as the form new code uses, with its real grammar, its real closer, and the checked words it generates, and place it above the older PRODUCT, SUMTYPE and ENUM entries it supersedes rather than beside them as an equal choice. Say plainly which of the older forms remain correct for which cases and which are legacy for new code. Keep the accurate statement about E-REMOVED-TYPE-SYNTAX. Correct the neighbouring lines that describe the type-declaration family, including line 425 and lines 460 to 489, so the whole section reads as one consistent account rather than a new paragraph bolted onto a stale one.

Do not describe behaviour that was not run. Every claim about what a declaration does, what it rejects, and what error it produces must be checked against the engine on the tree being edited.

Acceptance and smallest owning check: for each concrete claim the rewritten section makes, a command run through bin/hb on the same tree produces the stated result, and the specific false claims measured above no longer appear anywhere in docs/forth.md. The examples in the section load green.

Verify: run the section's examples through bin/hb, then host-lint.

Files: docs/forth.md only, unless a run proves an engine diagnostic is genuinely wrong, in which case stop and raise that as its own dot rather than documenting around it. Claim: unassigned.
