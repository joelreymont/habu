---
title: three entry forms for one build CLI
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.173270+02:00"
---

Problem: tools/build-fixpoint.f:2079-2082,2141 BF-CLI-SELF-DISPATCH, tools/build-fixpoint-main.f, tools/build-fixpoint-refresh.f all reach BF-CLI; BF-CLI-RAN (:2025-2031) exists only to stop double builds; bootstrap.sh:461-464 and seed.f:157-173 spell the 11-file preamble by hand; build-fixpoint.f:53-55 and -main.f die with the same message; sibling one-liners tools/check-main.f, seed-main.f, boot-pin-main.f, size-report-main.f, code-owner-main.f. Acceptance: one require-based entry (build-fixpoint-refresh.f), -main.f and the self-dispatch deleted, bootstrap.sh/seed.f call it; the *-main.f one-liners folded with a named-load guard. Files: tools/build-fixpoint*.f, tools/bootstrap.sh, tools/seed.f, the *-main.f files. Verify: recovery run; docs/bootstrap.md commands. Depends: none. Ownership: build tool. Claim: unassigned.
