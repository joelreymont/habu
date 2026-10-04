---
title: Seal the checker-owner entries from checked source
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T02:34:47.723513+03:00"
---

Problem: checked source can call checker-owner ABI words that only the engine should: on the 9084b558 release engine and on lane 521's g1, `: W ( -- ) s" A" CHECKER-OWNER:USIG-TRUNCATE ;` certifies and runs and afterwards A's records are gone (a later `: B ( n -- n ) A ;` is refused, undefined word A); CHECKER-OWNER:DECLARED-EFFECT, DOES-BEGIN and DOES-FINISH certify and run the same way. Found by review 558 (probes $HOME/.cache/tmp/kestrel-r4-rev558/pr/b1.f, b2.f, b3-base.f, b3-g1.f with their .out files). Fix: refuse every checker-owner entry to checked source by the mechanism lane 521 uses for its own new entries (private or REG-PROTECT, as TRUST-DECL and EFFECT-QUERY-SYM have); census the CHECKER-OWNER and NDICT words checked source can reach and state which each gets. Acceptance: b1-b3 refused (rc 70), each seen running first; the engine's own callers unchanged; baked: rebuild, g1 == g2, two-generation build. After: 7b3f85de.
