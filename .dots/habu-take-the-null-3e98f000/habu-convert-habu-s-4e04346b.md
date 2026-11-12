---
title: "Convert Habu's nulls to the maybe-null family"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:27:45.368095+02:00"
blocks:
  - habu-lower-niche-null-68a7b3ac
---

Lane 2 of /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md: one commit per file group (a public effect that becomes the maybe-null family plus its callers): a distance becomes the distance word, a test a MATCH, an empty string a real zero-length string, a store, return or C argument the empty variant. Acceptance per group: its suites pass and no NULL-PTR is left in its files. Depends: lane 1 landed and installed; lane 0's groups. Claim: unassigned.
