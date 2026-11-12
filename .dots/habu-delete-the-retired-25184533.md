---
title: Delete the retired-symbol paths nothing reaches
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:46:51.443151+02:00"
---

Problem: with CHECKER-SWEEP deleted (its DECIDE and SYM-RETIRE were the only retirers), nothing retires a checker symbol, so SYM-RETIRED? (src/core/checker.f) is true only for symbol 0 and the retired-row handling in NORET-COMPACT, RECORD-CTL-MAP (with its comment near :29197) and the other SYM-RETIRED? tests cannot fire. Acceptance: each such path is deleted or shown reached by symbol 0 and kept with that reason in its comment; hb rebuilds to a fixpoint. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/retired-sym.
