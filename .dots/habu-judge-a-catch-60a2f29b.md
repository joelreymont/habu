---
title: "Judge a catch on a loop's final throw facts"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T00:40:13.733673+01:00"
---

Problem: src/core/checker.f joins a loop-carried quotation's throw bit (CTL-THROW) and its intact masks in place on the back edge, after a catch in the body has read them, so the catch is judged on facts the loop later widens (found reading the quotation-facts design, /tmp/jerry-p1/br/qfacts-design-report.md; no reproducer yet). Acceptance: a reproducer shows whether a checked program's catch misjudges a quotation that gains a throw on a later pass; if it does, the catch is judged on the loop's final facts and the reproducer is rejected or passes as its effect says. Depends: none. Claim: unassigned.
