---
title: Hold text in str fields, not span<u8>
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:37:01.402087+01:00"
---

Problem: records keep text in span<u8> fields, so each store turns a str into a span and each read turns it back with SPAN:$: private >SPAN in src/lib/net/mcp.f:227 (ID, METHOD, PARAMS, TEXT); inline STR:DATA … STR:LEN SPAN:MAKE in src/lib/net/bearer.f and src/lib/fs-mutate-test.f. The public ENUM message in src/lib/net/ws.f:50-51 holds a received message as two fields, text (chars ^u8, count n) and binary (bytes ^u8, len n). Two helpers with one meaning, and conversions that carry no information. Acceptance: a field or argument that only holds text a word reads is typed str; no str-to-span helper or inline conversion remains for text; a region a word writes stays a span or becomes a vector. Depends: none. mcp.f goes with d3d98410. Claim: unassigned.
