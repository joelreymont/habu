---
title: "S5c census 4: what hb's checker refuses in the checker rows 14-22"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T04:02:55.924992+02:00"
---

Problem: census 3 and the checker-rows lead check (/tmp/jerry-ckrows/probe/) loaded rows 0-13 inside one build with checking on once checker.f's 167 top-level xt! registrations were commented out in a probe copy; row 14 src/core/engine-error-effects.f:6 stops rc 70 'no authenticated package context' because the product's TRUST asks its own empty package mirror (CHECKER-PKG-CONTEXT). What hb's checker refuses in rows 15-22 (through check-hook.f) is not known, so the checker-rows design (/tmp/jerry-ckrows/design.md) may misname their refusal classes. Acceptance: a report (no tree commit) naming each row 14-22 loaded inside one build on the probe hb with its time and each further refusal class with file:line and message, the probe copies' substitutions listed. Files: none in the tree; probe copies under /tmp/jerry-ckrows/probe/. Verify: /tmp/jerry-ckrows/probe/iter.sh log. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ckrows.
