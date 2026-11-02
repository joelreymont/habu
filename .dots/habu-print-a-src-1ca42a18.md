---
title: Print a source place through one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T14:10:08.062834+02:00"
---

Problem: four words print a source place (path:line) each in its own way: src/core/check-hook.f AT-SOURCE, src/habu/outer.f AT-SOURCE, src/compiler/native/definition.f SAY-WHERE, and the engine's habu2.f LSRCAT (the one C-PACKAGE-FAIL uses). Joel 2026-10-31 (LESSONS 'names'): helpers with one meaning are one word. Acceptance: the Habu-side printers share one word at their lowest common owner (or call the engine's), with the same output on each refusal they print; check-hook.f's goes with the checker switch (dot habu-remove-the-checker-4425bcef) if that lands first. Files: the four above. Verify: the refusal rows that print places (test/outer-interpret.f, tools/check-verify-test.f, test/compiler/*refusal*.f). Depends: none. Ownership: carl. Claim: unassigned.
