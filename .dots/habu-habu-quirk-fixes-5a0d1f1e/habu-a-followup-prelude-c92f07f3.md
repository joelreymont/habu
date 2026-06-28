---
title: "A-followup: prelude words the extractor rejects"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-06-27T13:30:20.603551+02:00\""
closed-at: "2026-06-28T16:07:33.731398+02:00"
close-reason: "Both blockers resolved by engine/tooling evolution (no engine change): public-signatures emits <=/>= names (F<=/F>= in JSON output) and dup/over certify with float r — fdup/fover/f<=/f>= all certify. Added the 4 words to lib/prelude.f (export+def), 4 rows to lib/std.manifest, tests to lib/prelude-test.f, removed the stale omitted-note. Gate green (warm 121736ms<=160000ms, fixpoint), 0 non-budget; prelude + stdlib-manifest suites ok."
---

Re-add f<=/f>= and fdup/fover to lib/prelude.f once public-signatures handles them: (1) it does not emit words whose NAMES contain <= or >= (parser/token filter); (2) fdup/fover bodies (dup/over) infer a generic cell 'a' not 'r', so the declared (r -- r r) mismatches. Fix public-signatures name handling + decide whether duplication words publish as generic or per-type, then add the words + manifest rows.
