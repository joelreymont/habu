---
title: "Build --repl images through LINKER's app mode"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.075315+02:00"
closed-at: "2026-11-09T16:20:43.762038+02:00"
close-reason: "landed with 'Build --repl programs in hb-build itself': hb-build --repl builds hb with the source's files inside LINKER:WITH (HB-BUILD-CORE:REPL), the source's MAIN its entry; app-build.f, app-build-image.f, app-image-engine.f, keyed-image.f's test and gate images go. catch-success-image-subject --repl build: cold 10.9 s, warm 2.9 s, equal after codesign --remove-signature, maps equal, the built hb prints catch-image: ok and catch-image-fresh: ok. catch-stale, linear, include-refusal, whole-source-read PASS; gate-entry-guard-test ok; next-hb h2 == h3."
---

Step 5c (/tmp/jerry-big/xbuild-order.md; design r2 3.8). Acceptance: hb-build --repl links hb's startup files plus the application's files through LINKER's app mode; the app-image-engine keyed image goes. Files: tools/hb-build*.f, tools/app-build.f, test/gate-images.f. Verify: the --repl image tests. Depends: 5a. Ownership: those files. Claim: unassigned.
