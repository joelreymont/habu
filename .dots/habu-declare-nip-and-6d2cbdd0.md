---
title: Declare nip and rot as HIR renames
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T23:20:40.124069+02:00"
---

Full context: docs/compiler-ir-design.md section 7.3 names NIP and ROT as stack renames that produce no operation, but src/compiler/native/hir-word.f REGISTER-WORDS declares only dup/drop/swap/over - and the native-hir suite currently uses rot as its undeclared-word NEGATIVE fixture (UNDEC-CASE), so declaring it flips that fixture. Add nip (consumes 2, puts back 0) and rot (consumes 3, puts back 1 0 2 - verify against the design), re-point UNDEC-CASE at a genuinely unmodeled spelling, extend the elaborator suite with a rot-using word asserting zero added operations. Before the optimizer leaf assumes the full rename set.
