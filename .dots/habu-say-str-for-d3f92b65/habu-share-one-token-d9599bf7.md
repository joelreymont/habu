---
title: Share one token reader between the check and the walk
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T10:31:21.946159+01:00"
---

Problem: the check (src/habu/verify-source.f SKIP-ARM, WALK-NAMES) and the loader walk (tools/source-discovery.f SD-CALL-ACT) each read declaring forms with their own token reader, so in a skipped [if] arm they still disagree on definition state (create inside a colon body), a declarer's name not consumed (create :), parsing keywords (postpone :, ' (), {: groups and family bodies, an unterminated string (the walk rejects it with E-UNTERMINATED-STRING where the engine and the check skip it), and at top level on .( : required ); called from the walk, DYNAMIC-REJECT reads CHECKER-SPLIT-STR of another file. Acceptance: one reader decides these for both, and check and walk agree on a reproducer of each case. Depends: none. Claim: unassigned.
