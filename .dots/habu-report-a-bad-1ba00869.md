---
title: Report a bad SELECT-LIBRARY path Rust-style
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T04:52:35.279579+01:00"
---

Problem: src/lib/ffi-abi.f PATH-ROOM, reached from public SELECT-LIBRARY, throws E-FFI-SYNTAX bare, with no record, for an empty path or one longer than LIB-PATH-CAP. Acceptance: each prints a Rust-style record (place, source line, the length found and the bound, the fix), as the declarer's rejections do. Depends: none. Claim: unassigned.
