---
title: Pin HBR2 wire fixtures and Wasm numeric goldens
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.874608+03:00"
---

Problem: PA-r2 §18 binds to HBR2 §24.5 (two imports, six exports, 128-byte control record, 96/32-byte headers, 136-byte STOP) and §19 to HBR2 §4.2 limits, but Habu has no fixture for any of them, and HBR2's codec registry (hbr-v2-registry.json) does not exist yet (Joel, 2026-10-03) (PA-r2 P0). Acceptance: the registry generated from HBR2's normative tables into lib/browser/, with its digest computed as sorted compact UTF-8 JSON excluding contentDigest and checked; golden tests for the control record, packet and record headers, STOP packet and callback limits; Wasm numeric goldens N01-N06 including the three NaN print rows (`-1 fsqrt`, `0 0 f/`, `inf inf f-`) and three ?do rows (`-1 0 ?do`, `MIN-N 0 ?do`, counting-down +loop), run natively now. Files: lib/browser/hbr2-registry.json (new), test/wasm/hbr2-fixtures.f (new), test/wasm/numeric.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f; a one-byte registry edit fails the digest test. Depends: habu-land-the-portability-dbde2246. Ownership: lib/browser/, test/wasm/. Lane: tim. Claim: unassigned.
