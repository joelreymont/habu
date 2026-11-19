---
title: "Check files that use CALLBACK: and VERSIONED-LIBRARY"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:01:17.709609+01:00"
---

Problem: tools/check.f leaves verification of a file to the run (W-CHECK-DEFERRED) where a definer has no parses: row: CALLBACK: (src/lib/pg.f:981) and VERSIONED-LIBRARY (src/lib/crypto/evp.f:68, src/lib/net/curl.f:176), so check.f --verify-only on pg.f, mcp.f, test-issuer-test.f and their callers proves less than it says. Acceptance: bin/hb --load tools/check.f -- --verify-only on src/lib/pg.f and src/lib/net/mcp.f prints no W-CHECK-DEFERRED. Depends: none. Claim: unassigned.
