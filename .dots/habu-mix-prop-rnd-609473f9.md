---
title: "Mix PROP:RND%'s low bits so small bounds are not periodic"
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T05:06:39.643614+03:00"
---

Problem: lib/property.f PROP:RND% with a bound of 2 returns the truncated LCG's low bit, which alternates with period two, so a coin drawn that way never yields two consecutive equal values (birch/Tender 2026-09-18: a generator drawing 'add another input?' with bound 2 never built a document with two inputs and hid a missing-separator defect until the coin came from a wide bound). A property-test generator whose small-bound draws are periodic is not random and hides exactly the cases it exists to find. Acceptance: RND% derives a bounded draw from the generator's high bits (or a mixed output such as an xorshift/multiply step) so that bound 2 and other small bounds produce runs, with a test that the sequence of bound-2 draws contains both repeats and alternations at the expected frequency, docs/forth.md's property-testing paragraph stating the guarantee; existing seeded expectations in the tree re-pinned where they change. Files: lib/property.f, lib/property-test.f, docs/forth.md. Verify: lib/property-test.f; test/run.f (any suite pinning a seeded sequence). Depends: none. Ownership: property testing library. Claim: unassigned.
