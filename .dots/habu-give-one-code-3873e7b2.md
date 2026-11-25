---
title: "Give one code for a linear cast's effect"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:33:01.888078+01:00"
---

Problem: ': W ( ^lin1 -- ) CAST ;' style word whose input is a linear value is E-CAST-LINEAR on the load and E-LINEAR-EFFECT in tools/check.f --verify-only at the same token (reproducer xt in /tmp/jerry-p1/ck6/kind/t while it stands: '( ^lin1 -- ) W'). Acceptance: one code at one place in both, with a diag-position case. Depends: none. Claim: unassigned.
