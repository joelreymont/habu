---
title: Remove synthetic compose separators
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-15T23:48:26.539815+02:00\\\"\""
blocks:
  - habu-compile-authenticated-src-05e058a2
---

Full context: tools/source-compose.f LOAD-ACTION inserts generated LFs around include/require and maps them onto the loader token, so diagnostics can invent user coordinates. Destruction review disproved the original flat-concatenation premise: no ordinary separator can preserve source-frame EOF for comments, parse-name, char, tick, definers, packages, exports, undefine, pending compiler parsers, or arbitrary parsing immediates. This leaf now follows habu-compile-authenticated-src-05e058a2 and hard-cuts flat composed source: convert SOURCE-COMPOSE into the canonical frozen dependency-plan/manifest producer, pass authenticated frames to the compiler provider, delete synthetic text injection and any obsolete flat SOURCE$/source-map/remapper surface, and preserve native original-file diagnostics directly from the active frame. Acceptance: direct-versus-framed behavior and failure identity match for every destruction reproducer plus adjacent/start/end loaders, CRLF/no-final-LF, empty, repeated include/require, transitive/package-private scope, cache digests, and mutation-after-freeze; no generated source byte or fabricated coordinate remains. Files: tools/source-compose.f/test, hb-build integration/tests, obsolete source-map/remapper files and registrations. Claim: agent=/root workspace=.jj-ws/habu-build-exact-modular-44f4c2dc.
