---
title: Declare a long chain of one-parameter families
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:33:03.445695+01:00"
---

Problem: 5000 structures pK<a> FIELD x pK-1<a>, used once as STRUCTURE puse FIELD y p4999<n>, exit 102 after 69 s with E-STACK-BOUNDS (data stack) in TF-BASE: the parametric width walk (src/core/type-family.f TFW-FAM-WALK, TFW-PRODUCT, TFW-NODE) recurses once per family and grows superlinearly. Acceptance: the chain at 5000 and 20000 declares and is used in time linear in the count, or is rejected at the family that passes a limit derived from the stacks; a test at the old failing count. Depends: none. Claim: unassigned.
