---
title: One authority for literal classification too
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T14:00:30.000000+02:00"
---

Follow-on minted at the literal-authority checkpoint (2026-08-12): after num-parse made the engine the VALUE authority (landed d994661f), the checker still classifies literals with its own ALLDIG?/FLODIG? mirror of the engine's language - kind and value have two authorities and E-NFEED-LITERAL is the drift alarm between them (now unreachable from production per the landing's report, and untestable because the forge computes kind from the bytes it hands over). Move classification onto num-parse (a token is a literal iff the engine's reader accepts it), making drift structurally impossible and retiring the alarm. CAUTION: this changes the checker's literal language for every checked file - a spelling ALLDIG? rejects but the engine accepts (or vice versa) changes meaning; sweep for such spellings first and treat each as a finding. Files: src/core/checker.f. Depends: habu-record-the-engine-79c570ed (landed).
