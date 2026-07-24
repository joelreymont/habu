---
title: Protect DECL-EVENT publication owners
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-24T16:57:44.992204+02:00\""
---

Why: DECL-EVENT owns mutable declaration publication state, but its public and private wordlists are absent from the protected-wordlist registry. User source can reopen the package or publish directly through either wordlist after the friend latch closes.

Source result: after every public definition exists and DEV-PART-INSTALL has installed the generated-declaration participant, register the private wordlist with prot-wid-add, register the public wordlist, then close the package. test/seal-package.f must prove package reopen, direct public-wordlist publication, direct private-wordlist publication, and qualified publication all exit with ENGINE-ERROR:SEAL-PACKAGE through the real load, stdin, and direct-subject seams before a dictionary record appears. Resolve both wordlists only through the production XREF namespace API.

Discovered prerequisite: test/decl-event-suite.f lines 246-955 currently reopens package DECL-EVENT, defines private test helpers, reads DEV and DEVTX state, and contains a raw field-serial mutation boundary. The exact three-line source seal makes that unchanged suite exit 84 before its lifecycle proof runs. Do not weaken the seal, add a test bridge, move registration later, or absorb the 700-line migration into this leaf. Before dispatch, separately freeze small public-path or source-mutation replacements for those internal test sections, land them, and prove the suite no longer reopens DECL-EVENT or resolves private owner names. This hardening is not a code dependency of provisional-ownership guards, ENUM finalization, constructors, or the global ENUM cutover; it must not block that inference-engine critical path.

Acceptance after the test migration lands: each publication path is red on the parent and exits 84 with the canonical diagnostic on the change; removing either registration makes its matching direct-wordlist attack fail; declaration-event lifecycle, hostile seal, protected-wordlist AOT/snapshot, candidate-validation, package-diff, typed-local, trust/inventory, and full native gates pass. Forbidden: declaration API or transaction edits, new public or trusted access, legacy declarations, another registry or state flag, aliases, forwarding words, runtime guards, copied state machines, private-storage assertions, or changes to read-only tick/bracket-tick/postpone behavior. UNDEFINE, raw XREF readers, execute, and other protected-wordlist sink hardening remain owned by habu-tfam-2b-iii-545a815f. Owner: src/core/decl-event.f and test/seal-package.f after the explicit test-migration prerequisite. Claim: unassigned.
