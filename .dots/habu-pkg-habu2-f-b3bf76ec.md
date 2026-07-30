---
title: Package habu2.f interpret-mode defining words
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:50:42.413763+02:00"
---

Why: the one remaining package-diff-lint finding class in engine work — habu2.f's interpret-mode defining-words section (~25 words, ~450 lines: C-DEFHOOK, EMIT-CREATE, C-CONSTANT, the CF-ENTRY dispatch registrations around habu2.f:4880, and the EMIT-* emitters) predates packages and defines globals, so any edited definition there trips the lint. Measured facts from the ptr-elem lane: packages DO work inside habu2.f (a trial package C-DEFINE built fixpoint-green), but the unqualified 'using' import does not survive into generated stage-2 source — the leaf must use QUALIFIED calls throughout. Behavior: move the section into one package with qualified callers; dictionary names baked into AOT/treeshake manifests must be re-verified. Owner: src/habu/habu2.f defining-words section. Dependencies: none. Acceptance: package-diff-lint accepts an edited definition in the section; fixpoint x2 byte-identical or explained relocation drift; full test/run.f green. First consumer: the checker fix already landed needs C-DEFHOOK's corrected signature maintainable without lint exceptions. Claim: unassigned.
