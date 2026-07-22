---
title: Infer HTTP request frame
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T13:24:23.745135+02:00"
blocks:
  - habu-infer-http-syntax-c88a2a5a
  - habu-add-bounded-host-b40b048f
---

Problem: fragmented socket reads must publish one immutable bounded HTTP request or a typed refusal, never partial state. Acceptance: consume HTTP-SYNTAX and an owned bounded host region through an explicit incremental parser value. Accept only HTTP/1.1 identity framing, exactly one valid Host, at most one Content-Length, no Transfer-Encoding, and a body exactly matching the declared length; reject duplicate Host, missing Host, conflicting or duplicate lengths, unsupported transfer modes, capacity overflow, malformed lines, trailing bytes, and premature end before publication. Fragmentation at every byte boundary and two interleaved parser instances produce the same frame. Failure leaves no published request and does not mutate a previously published frame. Public access uses one immutable request type whose spans cannot outlive its owned region. Files: request frame package, focused fixtures, Maki manifests and FILEMAP. Verify: every-split and one-byte feeds, exact capacity, alias and lifetime negatives, owning Maki slice, typed-local, package, host, filemap, trust, strict inventory, and dot gates. Dependencies: habu-infer-http-syntax-c88a2a5a and habu-add-bounded-host-b40b048f. Ownership: incremental request framing and immutable request publication only.
