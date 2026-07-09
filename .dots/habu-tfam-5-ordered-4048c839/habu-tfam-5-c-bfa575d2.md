---
title: "TFAM 5: C-quote/dot-quote loader + dynamic-span fail-closed"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-04T08:53:59.626268+02:00"
closed-at: "2026-07-04T18:44:14.683485+02:00"
---

Unsupported string openers (C\" / .\") before a loader word must reject fail-closed instead of being replayed as a different source string (census sec1: they have no loader path today). Stack-string loader forms record the loader word's call-site span plus a path-origin classification; if a tool requires byte-exact path-expression spans and only a dynamic stack value is available, reject fail-closed. Add C\"/.\" loader-form negative fixtures. Depends on event-log + span-capture dots.

RESOLVED (TFAM 5 redrive, commits 5f47a8f0..01b541a0): C\"/.\" openers before a loader reject E-DISC-OPENER at top level AND inside colon bodies (fixtures SDT-TEST-OPENER, SDT-TEST-BODY-OPENER). Dynamic stack-value loader paths reject E-DISC-DYNAMIC (SDT-TEST-DYNAMIC, SDT-TEST-BODY-DYNAMIC); per the decided design (docs/design-tfam-5-redrive.md section 4) they are NEVER recorded with a span/origin classification - fail-closed rejection replaces the classification clause, and in declared dynamic-tail manifest files the form is skipped without an event. Literal loader events carry byte-exact call-site token spans, locked by SDT-TEST-COLON-SPAN slicing the entry bytes at EVENT-TOK@ and comparing the token text.
