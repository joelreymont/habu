---
title: Give the verifier the FFI declaration records
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:54:27.431296+01:00"
closed-at: "2026-11-16T12:35:45.624840+01:00"
---

Problem: --verify-only prints nothing and exits 0 for most bad FFI declarations (/tmp/jerry-ffirec/p s-long, many, ptrarg, two, s-ints, s-flts, s-open, s-close); src/habu/verify-source.f FFI-STAGING (:3797) throws a bare E-FFI-ARITY (check.f prints E-STATEMENT-THROW -3500); for s-end, s-end0, s-nodash, s-dash2, s-noparen check.f prints the checker's signature record, not the loader's. Acceptance: the three readers print the loader's record for each fixture, rc 70. Depends: habu-write-a-placed-a652e82a. Claim: unassigned.
