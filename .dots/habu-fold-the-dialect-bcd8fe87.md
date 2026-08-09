---
title: "Fold the dialect's lookup so capitals compile"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-09T23:47:21.161672+02:00\""
---

Claim: agent=capsfold workspace=.jj-ws/habu-fold-the-dialect-bcd8fe87

hir-word.f REGISTER-WORDS interns each control word in exactly one case while the engine's dictionary matches case-insensitively, so a body spelling IF or BEGIN is refused E-HIR-UNMODELED for its spelling alone. Measured 2026-08-09 after the locals tranche: IF 108, BEGIN 29, DO 4 in the E-HIR-UNMODELED bucket over lib src/core src/compiler (~141 definitions; named in habu-complete-the-chain-5aab8cee section 3 with no leaf until now). Fix: a folding rule at the dialect lookup - no new rows, no new machinery. Acceptance: a body written in capitals compiles to the same module as its lower-case twin (compare the modules, not just both-compile); census rerun shows the IF/BEGIN/DO spellings gone; a fixture proves a NON-dialect word in capitals is still refused by name. Files: src/compiler/native/hir-word.f, test/compiler/native-hir.f or neighbour. Verify: native-hir/native-chain suites, census, full gate. Depends: none.
