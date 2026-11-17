---
title: Build HTTP Basic credentials
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:05:55.570705+01:00"
---

Problem: mark (SAP OData client) builds Authorization: Basic itself; it needs a Habu word turning a user and a password into a request's Basic credentials that leaves no copy of the password or user:password in memory Habu owns and rejects a user holding : (RFC 7617 section 2); spark:/tmp/mark-habu-path-basic.md. Acceptance: the word does so, and a test shows the wipe and the rejection. Depends: Joel's name for the word. Claim: unassigned.
