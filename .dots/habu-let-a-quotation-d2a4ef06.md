---
title: Let a quotation literal move a declared value
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T17:20:33.498566+01:00"
---

Problem: a quotation literal's input row binds dup/drop/swap's cells to plain vars, never MOVED, so a one-cell NEWTYPE through [: dup X ;] catch is rejected (src/core/checker.f ONE-CELL-MET, LAYOUT-BLOCK?); Zen's mcp.f:1331 hits it once CURL:handle is an address. Acceptance: /tmp/jerry-nt/repro.f checks; a literal that reads its input is still rejected. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-qvar.
