---
title: "stdlib-manifest: missing module row for lib/fmath.f"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T20:40:36.793648+02:00"
---

Pre-existing red found 2026-07-11 by the tfam-finale lane: tools/stdlib-manifest-test.f exits 76 with 'missing module row for lib/fmath.f'. lib/fmath.f (shared FEXP core) landed in d4668e983036 (AD DAG: host gradcheck + broadened op set) with FILEMAP rows but no lib/std.manifest module row (and no rows for its published words, e.g. FEXP) and no docs/stdlib.md section. Red on base c1c554fd BEFORE the wave-A finale commits (proof: jj file show -r c1c554fd lib/std.manifest has zero fmath rows). Owner: the AD DAG lane. Fix: add the '1<TAB>fmath<TAB>lib/fmath.f<TAB>module<TAB>...' row + published-word rows keyed to lib/fmath-test.f + docs/stdlib.md coverage, then re-run tools/stdlib-manifest-test.f green.
