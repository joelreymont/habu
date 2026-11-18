---
title: Spell byte buffers as ^u8 n, not str
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:19:40.379642+01:00"
---

Problem: master's str sweep spelled byte buffers as str: arm64 link.f:81 SITE-SIZE, wasm link-site.f:15 SITE?, kernel-format.f:46 BYTES+, elf32.f:92 CHECK-TABLE, :99 HEADER-CHECKED, :105 CHECK-INDEX, wasm leb.f:129 S32@ and :132 S64@, digest.f:39 SLOTS-COPY, zlib.f:207 DEFLATE-FOR and :213 INFLATE-FOR; a reader cannot tell a string from a buffer. Acceptance: each row says ^u8 n (or its own cells), the engine's bytes are unchanged. Depends: none. Claim: unassigned.
