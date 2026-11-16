---
title: Verifier rejects EXPORT outside the package wordlist
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:27:24.722400+01:00"
blocks:
  - habu-checked-src-can-d80dc074
---

Problem: in a package whose current wordlist is another, --load rejects EXPORT E-PKG-CONTEXT while tools/check.f -- --verify-only accepts it: checked source stores the current-wordlist cell directly (`package XP wordlist data-base 40 + ! EXPORT XG ;package`, /tmp/carl-spark/rf/r2p/g2-hit.f: load and check.f E-PKG-CONTEXT rc 70, verify rc 0; g1-miss.f: E-PKG-CONTEXT against E-EXPORT-UNDEFINED). Acceptance: --load, tools/check.f -- and --verify-only reject both probes with the same first record and rc 70. Depends: habu-checked-src-can-d80dc074 (the store is rejected statically). Claim: unassigned.
