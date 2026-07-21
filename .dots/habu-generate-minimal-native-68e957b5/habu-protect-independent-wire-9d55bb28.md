---
title: Protect independent wire goldens
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:39:15.210158+02:00"
---

Invariant: a serializer or renderer golden is an oracle independent of the production code under test. The competitive evidence literals are therefore correct: composing the expected bytes with the same production rendering words would make the test tautological and let a shared defect pass. The missing work is a repository-wide rule and proof that other codecs do not build expectations from their implementation.

Document in the Forth testing standard when exact literal bytes or separately owned fixture data are required, and distinguish independent goldens from ordinary expected values, malformed-input fixtures, and generated source strings. Audit serializer, parser, wire-codec, report, and snapshot tests for self-oracles. Replace any expectation built through the production renderer or parser with a literal or independently implemented fixture; retain readable literals even when long. Do not blanket-rewrite string fixtures into word composition.

Add a checked audit or narrowly provable lint for known codec test patterns, plus mutation tests showing that changing a production token, delimiter, field order, escaping rule, or version makes its golden fail. Preserve exact versioned bytes and test intent. Verify every touched codec and snapshot suite, documentation examples, package, host, file-map, and full native gates. Record classified false positives rather than hiding them with broad exclusions, and measure test-source and loaded-code impact so the safeguard does not create duplicated production renderers.
