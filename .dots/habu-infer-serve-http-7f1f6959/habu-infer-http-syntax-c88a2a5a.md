---
title: Infer HTTP syntax
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T13:24:14.513423+02:00"
---

Problem: HTTP framing needs one exact, reusable grammar for request lines, field names, field values, decimal Content-Length, and case-insensitive field identity. Acceptance: add package HTTP-SYNTAX with checked stateless words over explicit counted spans. Return typed result unions for valid values and expected syntax refusals; throw only bounds or internal invariant failures. Enforce HTTP/1.1 request-line spacing, token field names, legal field values, overflow-safe decimal length, and case-insensitive Host, Content-Length, and Transfer-Encoding identity. No parser cursor, storage, publication, response state, raw trust, or mutable globals. Tests call the production words and cover exact boundaries, every forbidden byte class, decimal overflow edges, mixed case, duplicates as caller-visible identities, and adversarial comments or strings cannot satisfy the gate. Files: one syntax package, focused test, Maki manifests and FILEMAP. Verify: focused suite, owning Maki slice, typed-local, package, host, filemap, trust, and dot gates. Dependencies: none. Ownership: HTTP lexical and single-line grammar only.

Claim: agent=httpsyntax workspace=.jj-ws/habu-infer-http-syntax-c88a2a5a
