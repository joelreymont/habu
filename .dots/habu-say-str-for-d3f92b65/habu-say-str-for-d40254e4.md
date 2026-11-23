---
title: Say str for rejection and test helper words
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T15:51:49.903349+01:00"
closed-at: "2026-11-23T10:11:21.484389+01:00"
---

Problem: REJECTION BUFFER$ LABEL!, the E2E-TREE helpers and DECL-DIAG CAUSE? keep ^u8 n rows because unmarked tests call them. Acceptance: those rows say str, the unmarked tests drop their >str and str> around them, the engine builds and their suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rejh
