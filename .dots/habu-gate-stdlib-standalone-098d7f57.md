---
title: gate-stdlib standalone lint-tools slice missing lib/fs.f
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:39:10.814705+02:00"
---

Pre-existing break found by the maki-ns worker 2026-07-05 (present with their edits reverted): 'bin/hb --load test/gate-stdlib.f -- lint-tools' fails E-UNDEFINED: FS-PATH-CAP because test/gate-stats.f is loaded without lib/fs.f in that standalone entry's setup. The resident test/run.f path loads deps correctly and is green. Fix the standalone entry's load set in test/gate-stdlib.f (or gate-stats.f's requires) so the slice runs; add the slice to whatever proves standalone entries stay loadable. Owner overlap: same file family as habu-gate-case-lint-06257524.
