---
title: "Pass 16-byte scalars, vectors and HVAs through FUNCTION:"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T05:31:59.756589+01:00"
blocks:
  - habu-pass-records-with-af1b7f72
---

Problem: FUNCTION: (src/lib/ffi-abi.f) has no 16-byte scalar type (__int128, Linux quad long double, a vector), as an input or as the output, so neither such a value nor a record aligned to 16 bytes nor an HVA (vector members, passed in v registers) can be declared. Acceptance: tests call C functions taking and returning an __int128, a 16-byte-aligned struct and an HVA, by AAPCS64's and Apple's rules. Depends: habu-pass-records-with-af1b7f72. Claim: unassigned.
