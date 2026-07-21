---
title: Format MATCH arms separately
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:39:55.157075+02:00"
---

Invariant: each MATCH arm is one visible unit: one variant, one OF body, and one ENDOF on its own physical line. Dense lines with several arms hide exhaustiveness, make review and diffs error-prone, and appear across production, tests, tools, and checked fixture source. A lexical census found 635 candidate lines in 72 files containing at least two uppercase OF tokens; the confirmed violations span string, CAD numeric, CAD effect, and Maki execution code.

Add the exact rule to the Forth standard, produce a tokenizer-aware census of MATCH blocks, and reformat every violation without changing token order or behavior. Add a checked source-layout lint that recognizes actual MATCH arms and ignores comments, strings, identifiers ending in -OF, lowercase Forth case syntax, and generated payload text unless that payload is itself a checked fixture subject to the rule. Empty and throwing arms still receive their own line.

Prove positives and negatives for comments, strings, INDEX-OF-like identifiers, compact whole-definition MATCH code, nested MATCH, generated checked subjects, and two real arms on one line. Run every touched exact load, MATCH, declaration and type-family suites, formatting and typed-local lints, bootstrap, fixpoint, Maki, PTX standard library, and full native gates. Compare source tokens, loaded JIT, DATA, CODELEN, and emitted behavior before and after; formatting must be runtime-byte neutral.
