---
title: Rename suite DSL END-GROUP/END-SUITE to ;GROUP/;SUITE
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:18:24.260086+02:00"
---

User rule: block pairs are FOO … ;FOO. lib/test/suite.f:343 END-GROUP -> ;GROUP (closes GROUP-PARALLEL/GROUP-SEQUENTIAL), :355 END-SUITE -> ;SUITE, :159 END-SUITE? predicate -> ;SUITE? (token recognizer — verify what token stream it scans and rename the scanned token consistently). Sweep ~128 occurrences in 8 files: lib/test/suite.f, lib/test/suite-test.f, lib/std.manifest, maki/test.f, test/gate-stdlib-cases.f, docs/stdlib.md, docs/forth.md, LESSONS.md (leave LESSONS history lines intact if they narrate the old name as history). Gates: bin/hb --load maki/test.f, ptx-stdlib slice, native suite, host-lint, filemap-lint. SEQUENCE: after tfam-6/tfam-5p merge (maki/test.f + gate-stdlib overlap).
