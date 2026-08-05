---
title: "Bind the checker's accepted effect to the recorded unit"
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T19:07:52.370461+02:00"
---

Full context: src/compiler/native/elaborate.f NELAB:COLON still takes the definition's declared in/out counts as its last two arguments (its one remaining told-not-read fact, plus the fixed exported visibility), and test/compiler/native-chain.f states them at one line for the definition it compiles. The checker already HAS the answer at exactly the right moment: CHECK! parses the declared signature during the very scan the tape is recorded from, and at the CHECKER-TAPE:DONE event SGIN/SGOUT hold the verified declared rows, whose physical widths ROW-CELLS answers. What the checker publishes today instead is EFFECT-QUERY (src/core/checker.f, TRUSTED, reachable only from an unchecked window), which resolves an effect by NAME against the live store - so it answers about whichever word carries that spelling when the call is made, not about the definition a given tape is, and a redefinition between check and elaboration silently changes the answer. That is a binding by timing, not by structure, and it must not be built. Acceptance: the verdict event carries the accepted effect's fixed din/dout cell counts alongside the verdict, NFEED records them, NCERT:result publishes them bound to the same tape and text digests it already binds, NELAB:COLON reads them off the result instead of taking arguments, a definition whose body disagrees is refused against that recorded effect, and native-chain.f states no arity at all. Depends on habu-bind-the-colon-ea509e61 and habu-bind-checker-env-ed4f9f87; discovered while landing habu-reconcile-the-produced-26737779.

GROOMED 2026-08-04 (dot-groom). Stale reference corrected.
habu-reconcile-the-produced-26737779, named above as the landing during which this defect was
discovered, is no longer in the graph: it was closed and archived by commit 68e14fdf3 "Close
the tape-reconciliation dot". It was provenance, not a blocker; the two real dependencies
(habu-bind-the-colon-ea509e61, habu-bind-checker-env-ed4f9f87) still exist and are
untouched.
