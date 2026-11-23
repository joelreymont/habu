---
title: Reject a vector view kept across growth
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.747630+01:00"
---

Problem: VEC:SPAN's span, and a str made from it with SPAN:$, stays usable after VEC:PUSH, VEC:APPEND or VEC:RESIZE moves the block or VEC:DELETE frees it (src/lib/vector.f:54 GROW, :129 SPAN). The checker accepts a word that takes `v VEC:SPAN SPAN:$`, pushes 100 bytes onto v and types the str: a read of freed memory. Replacing SB with caller-owned vectors (design .out/designs/design-sb.md, 'Hazards a brief names') multiplies such views. AGENTS.md: no runtime checks, so the checker must reject it. Acceptance: the checker rejects a span or str taken from a vector and used after a word that may move or free that vector, in the Rust-style form; a view used before any such word still passes. Depends: none. Design: .out/designs/design-vecview.md (names open). Claim: unassigned.
