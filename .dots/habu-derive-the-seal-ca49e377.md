---
title: Derive the seal from a classification, not the rewind
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:17:55.795908+03:00"
---

Problem: about 1987 records carry DNAME-INT because CHECKER-BOUND:MARK rewinds the checker registry before src/core/internal-mark.f runs, not because anything classified them: on a booted engine CHECKER-RESOLVES? answers NO for every prefix package below the core-prefix mark (TFAM:REG-AOT-MERGE-INCOMING?, TFAM:TFAM-N@, CHECKER-BOUND:REWIND, LOWER-CERT:ARENAS-STALE, CHECKER-REG:DECLARATIONS) while PREFIX-MARK:*, the last file before the mark, resolves (strip lane, 2026-09-17). Every name-strip and internal-word decision hangs off that accident, and the build chain's own TRUSTED: sites (habu-give-the-build-chain dot) exist to reach across it. Acceptance: the internal set is stated by a rule about packages and words that survives the rewind, recorded in docs/bootstrap.md next to the prefix-rewind description, with a measurement of which records change class; user programs still cannot name any of them; internal-word-gate agrees. Files: src/core/internal-mark.f, src/habu/prefix-rewind.f, the CHECKER-BOUND package, docs/bootstrap.md. Verify: a probe over CHECKER-RESOLVES? for the five names above; test/internal-word-gate.f; test/run.f. Depends: none. Ownership: engine seal. Claim: unassigned.
