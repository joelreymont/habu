---
title: Reject stale FILE before storage preparation
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T19:13:16.211852+03:00"
---

The standalone strict-object E2E packs a valid UNIT, changes the checker owner, and confirms NUNIT-ORIGIN:ELIGIBLE? is false; its rejected FILE export then executes unbound HOST-STORAGE and exits 86 instead of throwing E-NUNIT-PROFILE. Reject this owner before storage preparation, retain automatic false results and pointer/binding refusals, and close the CWORK range even when validation throws. Independent review reproduced a caught PACK failure after FILE-MARK and an untracked TAKE that leaves FILE-MARK throwing 7121; add this distinct regression before moving FILE-ACTIVE's reset ahead of fallible export work. Acceptance: strict-object, prepared, state, task, CWORK, admission and explicit-unit E2Es on a source-compatible engine with fresh external HB_TMP. Worker: carl-native-file-fixes; keep active until review and prerequisite integration permit landing.
