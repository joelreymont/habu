---
title: "Check files that use VERSIONED-LIBRARY"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:01:17.709609+01:00"
---

Problem: tools/check.f leaves a stretch to the run ("reads its operands at run time", "a stretch or definition deferred to the run is not verified") at VERSIONED-LIBRARY (src/lib/pg.f:221, src/lib/crypto/evp.f:69, src/lib/net/curl.f:193), so check.f --verify-only on pg.f, evp.f, curl.f, mcp.f and their callers proves less than it says. Acceptance: bin/hb --load tools/check.f -- --verify-only on src/lib/pg.f and src/lib/net/mcp.f reports nothing deferred. Depends: none. Claim: unassigned.
