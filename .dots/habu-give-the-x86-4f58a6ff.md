---
title: Give the x86-64 kernel source-exec-run
status: active
priority: 2
issue-type: task
created-at: "2026-11-01T16:22:27.968107+02:00"
---

Problem: src/habu/kernel-x64.f ~2596 registers source-exec-run, source-exec-live? and source-exec-boundary? with REFUSE-WID, so SOURCE-EXEC:RUN (src/habu/source-exec.f) dies with 'hb: source-exec-run is not in the x86-64 kernel'. On master efef8267's x86 engine (joel@pulsar-ua-106, full suite /tmp/jerry-big/x86base-suite.log) that message ends these rows: cell-body-image compiler-images-build compiler-native-hookless-reject dictionary-capacity ffi-callback-file-cache ffi-callback-zero-image fixture-writer-build hb-build-aot-image hb-build-cli-errors hb-build-producer-cache hb-build-repl-fresh hb-build-repl-image hb-build-repl-twin hb-build-stripped-cache native-file-alias native-file-dynamic-load native-file-forms-global native-file-forms-namespace native-file-forms-repeat native-file-forms-zero native-file-prefix native-file-replay native-file-state native-file-stripped-path native-file-tree-share native-file-types native-gate-aot-positive-bundle stripped-entry stripped-image stripped-literal stripped-pool-reach whitebox-engine-key xml-owner-consumer zlib, and every application build on x86 (hb-build, app images, incremental native files). The arm64 twin is habu1.f BSOURCEEXECRUN (~5032); src/habu/outer.f ~593 states its frame contract. Acceptance: the x86 kernel implements the three primitives as arm64 does; those rows pass on the x86 peer or fail for a different, recorded cause. Files: src/habu/kernel-x64.f, src/habu/boot-x64.f if its observer state needs clearing. Verify: x86 cross-build, then the listed rows on joel@pulsar-ua-106. Depends: T0 installed on x86 (habu-install-t0-as-6492133d), since its rows build at tier 0. Ownership: kernel-x64.f source-exec rows. Claim: jerry, lane sx86 in .jj-ws.noindex/jerry-sx86 (brief /tmp/jerry-big/sx86-brief.md rev 3).

tools/two-generation-build.f on x86 stops at generation 1 with the same message, on master efef8267's engine and on the T0 x86 stack alike (pulsar /tmp/jerry-x86base/gen.log, rc 1).
