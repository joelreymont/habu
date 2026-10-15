---
title: Move JS and C peer asserts into Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.219282+03:00"
---

Problem: test/wasm/data-peer.mjs:18-32 makes 12 asserts that Habu could make. test/browser/request-peer.mjs (353 lines, 79 asserts) is not in the CLAUDE.md exception list. test/ffi-f32-peer.c is run by hand and never gated, and the mixed int/float and variadic FFI cases exist only there. Acceptance: the peers only stand in for the foreign side, and the asserts move into the Habu tests; the f32 mixed and variadic cases are gated (built by the test harness or covered through an existing gated FFI fixture library), and the manual C file is deleted or registered. Files: those named and their Habu drivers. Verify: wasm, browser and FFI rows. Depends: none. Ownership: listed files. Claim: unassigned.
