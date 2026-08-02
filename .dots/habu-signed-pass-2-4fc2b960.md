---
title: Signed pass-2 xpad corrections through the trusted certificate
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:42:35.837742+02:00"
---

Problem: habu-fail-closed-on-0ab1e401 made asymmetric-growth instantiations (widest-declared variant != widest-instantiated) fail closed at construct/MATCH via CONSTRUCT-WIDE-STAGED-REJECT — sound but limiting: such a family is unconstructable in its negative-extra variant and (via MATCH exhaustiveness) unmatchable at that instantiation. Full support requires signed extra-pad facts: the width-fact certificate format and the trusted native validator VALIDATE-WF in src/habu/habu2.f must carry and accept negative corrections, and the native emitter must remove cells, not only add. Keep the validator's rationale source-local, name its retirement owner, and prove the asserted certificate through the focused runtime depth test. Include cleanup: CONSTRUCT-WIDE-STAGED-REJECT is now shared by construct and MATCH-arm reject paths; neutralize the name (e.g. WIDE-STAGED-REJECT) and repoint the deferred-capability comments in src/core/type-family.f to this dot id. Acceptance: the asymmetric fixture family in test/type-ctor-suite.f constructs and matches with certified width equal to native bundle width for both variants; runtime depth probes agree; negative regressions reject genuinely mis-sized bundles. Files: src/core/type-family.f, src/core/checker.f, src/habu/habu2.f, and test/type-ctor-suite.f. Verify the type-ctor and type-match suites, focused certificate/depth tests, fixpoint, and the full native gate. Depends: habu-fail-closed-on-0ab1e401 (merged). Ownership: width-fact certificate signed-correction capability.
