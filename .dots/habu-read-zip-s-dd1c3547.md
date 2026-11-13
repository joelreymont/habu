---
title: "Read ZIP's little-endian fields through LE"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:34:39.778745+02:00"
---

Problem: src/lib/zip-raw.f:69-78 defines its own LE@, LE!, U16@ to U64! that shift and mask byte at a time, beside src/lib/le.f's LE package, which every other little-endian field in src/lib/ reads through. Acceptance: ZIP reads and writes its fixed-width fields through LE's words, its own copies deleted, and its width-generic uses (:128-159, :369) stated at their widths; src/lib/zip-test.f passes. Depends: none. Claim: unassigned.
