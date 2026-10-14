---
title: Test the storage refusal of a type scheme
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-02T10:36:17.433100+02:00\""
closed-at: "2026-10-14T19:44:52.849512+03:00"
close-reason: 0e595aa6 added a named scheme-in-storage refusal; current c2-memory-scope-refusals.f asserts the scheme reason for typed globals, buffers and layout definers. docs/type-families.md now describes E-BAD-STORAGE, rc 70.
---

Problem (r4-mastermerge worker 3): master's test/c2-memory-scope-refusals.f:74 claims a storage declaration refuses a type scheme ('a scheme cannot be stored'), but the refusal it observes is 'malformed type forall<p,[': STORAGE-PARSE-TYPE reads a single token, so the scheme never reaches the storage rule. Same on master's engine. The row passes for the wrong reason and pins nothing about schemes. Acceptance: decide from docs/type-families.md whether a scheme is a storable type spelling; if it is not, the declaration is refused by the scheme rule with its own named refusal (multi-token type spellings parsed as the checker parses them elsewhere), and the row asserts that text; if a scheme can be spelled for storage, the parser reads it and the row asserts the real outcome; failing case first. Also docs/type-families.md:2081 says an inadmissible storage type throws E-LAYOUT-BUFFER, stale since the line's named rc-70 refusal (c6a2ae46): correct it in the same commit. Base: after the master merge (the test is master's).
