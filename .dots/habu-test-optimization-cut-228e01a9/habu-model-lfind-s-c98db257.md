---
title: "Model LFIND's x5 record result in clobber-lint"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:34:53.863509+03:00"
---

Found by lane 532 (dot 8a8912cc): src/habu/habu1.f:4834-4840 says Lfind, Lfindused and Laotwidgate return the record in x5, but tools/lint/clobber-lint-core.f:132-133 RETURNS-MASK lists Lfind and Lfindused as x11/x12/x13 and :175 PRESERVE-MASK lists Laotwidgate as x11; none has x5. 305ed456 closed on its START-L? qualified-label fix without adding x5; its 2026-08-16 note says the seed's patch pass works around the stale model. Acceptance: x5 in those three rows; a fixture caller that reads LFIND's x5 lints clean and is flagged when the row is reverted (seen failing first); clobber-lint and clobber-lint-test rc 0; the habu1.f comment names this dot no more and points at the rows in clobber-lint-core.f.
