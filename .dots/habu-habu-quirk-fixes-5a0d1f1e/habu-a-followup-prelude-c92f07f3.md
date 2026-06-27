---
title: "A-followup: prelude words the extractor rejects"
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T13:30:20.603551+02:00"
---

Re-add f<=/f>= and fdup/fover to lib/prelude.f once public-signatures handles them: (1) it does not emit words whose NAMES contain <= or >= (parser/token filter); (2) fdup/fover bodies (dup/over) infer a generic cell 'a' not 'r', so the declared (r -- r r) mismatches. Fix public-signatures name handling + decide whether duplication words publish as generic or per-type, then add the words + manifest rows.
