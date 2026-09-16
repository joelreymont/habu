---
title: Let residency skip loads the if-conversion elides
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T20:18:48.725493+03:00"
---

Problem: the combine pass could not be deleted (build-once lane, 2026-09-16, 84641c4b): its one surviving rewrite removes a data-stack load whose cell nothing reads, and regalloc-verify refuses such a load by name (E-A64RAV-DKEEP), so without it SAME-BRANCH (native-literals.f) and DROP-PREFIX (native-case-subject.f) do not compile. Root cause in selection: residency decides which cells a block enters holding BEFORE if-conversion decides which comparisons it will write, and A64SEL:REGION-PICK writes no comparison when a conversion's arms all hand the join the same value, so the cell is loaded for a reader that is never written. The collapse shortcut cannot simply go: it is also the path for memory-order positions. Acceptance: residency knows which readers the conversion will elide (or the conversion runs before residency for the region), no unread data-stack load is ever emitted, src/compiler/native/prune.f and package A64PRUNE are deleted with their suite converted, the two named fixtures and every native-* suite green, emitted census byte-identical or better, tools/compile-floor.f numbers before and after (prune costs 4.5 us on a word it does nothing to), byte fixpoint. Files: src/compiler/native/select.f, prune.f (deleted), compiler.f, test/compiler/. Verify: the compiler suites; census; compile-floor; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: tier-1 selection. Claim: unassigned.
