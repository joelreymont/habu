---
title: Run the native-window fixtures in one tier-1 child
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.032975+02:00"
closed-at: "2026-09-29T20:10:10.486404+02:00"
close-reason: Integrated merged native-window tier-1 fixture child; focused owner suite and pooled native gate passed 510/510.
---

Problem: each tier-1 native-window source case compiles the whole core prefix (32 s); 7 such children across native-window-owner/-source/-boundary/-payload, and compiler-native-checker-prefix spawns the same cast-ok child. The child already takes extra fixtures in argv order. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L6-test-g-z.md native-window finding and ~/.cache/tmp/kestrel-gate/test-review/L1-compiler-native.md finding 2. Acceptance: one merged fixture list per tier-1 child (fresh-owner checks first) keeps every assertion; checker-prefix folds into it; rows re-registered so none passes 180 s pooled. Files/Ownership: test/native-window-*.f, test/compiler/native-checker-prefix*.f (whichever file that row runs), their registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: agent=kestrel/worker workspace=.jj-ws/habu-run-the-native-e96fcb89
