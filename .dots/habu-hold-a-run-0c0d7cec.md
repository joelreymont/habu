---
title: Hold a run of bytes inline in a record
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T16:27:26.923123+02:00"
---

Problem: a STRUCTURE field holds only whole-cell values, so a record cannot hold a fixed run of bytes inline (omabox's hasher fakes one with cell padding records, three casts and a load-time size check; sha256.f's context is a bare byte count). Acceptance: `BUFFER name N` in a STRUCTURE (N a number literal > 0) lays out N bytes inline in whole cells; under DERIVE addr its accessor answers SPAN:span<u8>; span.f loads before sha256.f; sha256.f declares its context as a record; the language docs state the clause; a record with a buffer refuses DERIVE eq, hash and init. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-hold-a-run-0c0d7cec.
