---
title: Resolve http(s) URL references
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.966928+01:00"
---

Problem: mark (SAP OData client) asks Habu to resolve a reference against an http(s) base per RFC 3986 5.2, exposing the target's scheme and host, and to percent-encode a query component (space as %20, never +); examples in spark:/tmp/mark-habu-url.md. Acceptance: package URI words do both and the examples pass. Depends: Joel's names for the new words. Claim: unassigned.
