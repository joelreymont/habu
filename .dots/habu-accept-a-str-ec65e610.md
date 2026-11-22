---
title: "Accept a str target in a names: row"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:28:55.421483+01:00"
---

Problem: src/core/check-walk.f:2913 STRING-IN? reads a names: target's written row, so a split-str word whose row says str is rejected E-NAMES-ROW (src/habu/xref.f:326 XREF-FIND cannot say str). Acceptance: a names: row accepts a target whose input row ends in str or ^u8 n; XREF-FIND says str; the help text, card and forth.md say str; the engine builds. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-names.
