---
title: Reject a missing source in the verifier through the loader
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:11.004462+01:00"
---

Problem: --verify-only prints an old packet at require then a plain line for a missing source; the child resolves with SOURCE-ROOT:RESOLVE. Acceptance: one record at the path token, rc 70; the LSP keeps what it needs. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-loadrecv.
