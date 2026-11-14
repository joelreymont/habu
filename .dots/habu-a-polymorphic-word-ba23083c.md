---
title: A polymorphic word refuses an ENUM value
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T16:34:40.557299+01:00"
---

Problem: `: SW ( a b -- b a ) swap ;` certifies, and `( n n -- n n ) SW` checks, but `: U3 ( n -- ph n ) construct ph busy SW ;` with ph an ENUM is refused: in u3: at 'SW' expected: a b actual: n ph (also `( ph -- ph ph ) dup SW`), on the installed engine; swap itself takes the same values. Reproducer /tmp/carl-swap/q-U3.f. Cause: src/core/checker.f LAYOUT-BLOCK? refuses every layout-family value against a plain type var at a call of a checked word; only stack words and locals pass one. Acceptance: a parametric word whose body only moves its var takes a one-cell ENUM or sum value, as a test; one whose body reads the cell (`=`), a two-cell value and a possibly-linear value stay refused; a warm build equals the cold one. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-enum.
