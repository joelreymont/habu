---
title: Rebase, gate and push the integration stack
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.418052+03:00"
---

Problem: final step of the parent. jj git fetch; jj rebase -s vznuxmnw -d master@origin (rebase lanes with -r/-s, never -b); ARM gate in jerry-land (native-build, two-generation-build, cold aot-wid-build, x86 cross-build, bin/hb --load test/run.f; read each rc on its own line; gate and push never in one chain); jj bookmark set master/intel -r <tip>; push; then the final pulsar run on ~/Work/habu with the cross-built hb (pattern ~/.cache/tmp/jerry-c/pulsar/final-c-run.sh). Report to Joel every test change (list in lane-queue.md and the stack's commit messages) and the earlier pkill -f incident on pulsar. Depends: habu-review-and-land-2437d9b0 habu-review-and-land-5ce7ebe9 habu-land-the-64-1111310e habu-review-and-land-d6a57b7c habu-give-runtime-regression-cf20fae7 habu-finish-the-x86-5a565370 habu-unblock-x86-inline-a7682014 habu-implement-x86-native-261ee8bb habu-recover-x86-throws-b66ec686 habu-fix-remaining-x86-5a0c47fd. Ownership: lead. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.
