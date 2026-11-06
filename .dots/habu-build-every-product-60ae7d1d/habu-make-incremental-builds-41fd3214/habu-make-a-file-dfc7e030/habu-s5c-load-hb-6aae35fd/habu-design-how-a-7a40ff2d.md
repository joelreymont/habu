---
title: "Design: how a build loads the product's checker rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T03:37:37.721706+02:00"
---

Problem: S5c census 3 (/tmp/jerry-s5c3/report.md) shows hb's checker checks a build from row 0 while hb's startup loads boot-files.f rows 0-21 before check-hook.f installs the checker: hb's checker refuses checker.f:12352's xt! registrations (refused in a plain program too, /tmp/jerry-s5c3/r/r1.log), and the product's checker code that runs at its files' top level (engine-error-effects.f:6 TRUST) asks a package mirror the loop never updates (rc 70 'no authenticated package context'). No design (S5b3, S5b2-2/3, seal) says how a build loads the product's checker. Acceptance: a Plan design (decisions only, path:line, program paths, evidence) at /tmp/jerry-ckrows/design.md; its approach and every public item or significant change go to Joel, one question each, before any lane. Files: none in the tree. Verify: the design's probes under /tmp/jerry-ckrows/. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ckrows (Plan, read-only; brief /tmp/jerry-ckrows/brief.md).
