---
title: Correct comments that say a quotation cannot read a local
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:02:03.659543+02:00"
---

Problem: T0 compiles a quotation that reads its word's locals on arm64 and Wasm, yet comments still say it cannot, some beside globals kept only as that workaround. `rg -i 'quotation cannot (read|reach|capture)'` finds them in src/core/sumtype.f, src/habu/layout.f and src/habu/verify-source.f. A comment whose quotation goes to a task stays true (E-LOCAL-ESCAPE). Acceptance: each such comment is true (corrected or removed, by forth-card '9 Comments'); a global that exists only as the workaround becomes a local read by the quotation. Depends: none. Claim: unassigned.
