---
title: Seal generated constructors on validated families
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T12:29:28.407314+02:00"
---

Full context: found while building src/compiler/target.f. A public STRUCTURE family's generated MAKE is a forgery route: CTARGET-CONTRACT:MAKE can assemble any five field values without passing through the validating constructor CTARGET:CONTRACT, so a record can exist that VALIDATE would have rejected. The current defence is that every identity-bearing word (ENCODE, DIGEST, SAME?, BIND) revalidates its input and the suite proves a forged record throws instead of producing an identity - that is defence at the boundary, not a structural seal, and every future family with a validating constructor must remember to repeat it. Required result: a checker/type-system capability that lets a family's generated MAKE be private to its owning package while the TYPE stays nameable and usable across package boundaries, so validation cannot be bypassed by construction. Acceptance: a cross-package MAKE on a sealed family is rejected by the checker before runtime, with a negative checked fixture; the type remains usable as a field, parameter and return across packages; src/compiler/target.f, numeric-policy.f and binding.f drop their defensive revalidation where the seal now covers it, and their suites stay green. Note the existing threat model: the seal only has to hold against checked Habu, so 0 set-check and patch32 forges are out of scope - demand a checked-forge negative test.
