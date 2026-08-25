---
title: "Habu: blank line between multiline words in aot-capture"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T17:59:14.027867+02:00"
---

User FIXME src/habu/aot-capture.f:44 (main worktree, file currently dirty in the TFAM claude's tree - apply after their work lands or in their tree): multiline word definitions should be separated by a blank line, e.g. between ACAP-W32@ and ACAP-W32!. Mechanical formatting pass over aot-capture.f (and check siblings aot-*.f for the same pattern); no behavior change; gate = engine refresh + test/run.f since src/habu is fixpoint input. Style: docs/forth.md readability.
