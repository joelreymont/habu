---
title: A TRUST of a checked word weakens its proven facts
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:20.002324+02:00"
---

Problem: TRUST (src/core/checker.f:19698) amends a word's record through STORE (:15774), which takes a created effect only from the latches a body check sets (DOES-EFF-TAKE, WRAP-TAKE), so a TRUST writes creates 0: a TRUST naming a definer whose checked definition carries a created effect erases that effect, and callers and top-level uses then see the weaker row. No TRUST row in the tree names such a definer now, but nothing rejects one. Acceptance: a TRUST about a word whose effect the checker proved never weakens it; a type-rule test shows a definer's created effect surviving a TRUST of it, or the TRUST rejected. Asking Joel is required if it changes what TRUST accepts (a public word's behavior). Depends: none. Claim: unassigned.
