---
title: Refuse two builds sharing one HB_TMP
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.415221+03:00"
---

Problem: two concurrent tools/native-build.f runs that share one HB_TMP both die with `duplicate definition: IMAGE-BASE` rc 78 (table-fill lane, 2026-09-16), the same message the corrupted host engine gave, so a shared temp directory masquerades as a source defect. Acceptance: the build takes a per-run subdirectory under HB_TMP (pid plus a random suffix, removed on success) or refuses by name when it finds another run live there (a lock file with the owning pid, stale locks reaped); a fixture starts two builds on one HB_TMP and shows both succeed or one is refused by name; docs/bootstrap.md states the rule. Files: tools/native-build.f, tools/native-build-core.f, src/habu/build-*.f if the temp naming lives there, docs/bootstrap.md, test/. Verify: the fixture; test/run.f. Depends: none. Ownership: build driver. Claim: unassigned.

Claim: alder, .jj-ws/alder-build-temp on 1afd910c; measure the old failure before
changing current code. The current native driver captures its target in memory,
writes to the explicit output's .native-build.tmp, and uses TMPDIR-MKDIR for a
unique smoke directory which CLEANUP-RUN removes. No shared HB_TMP source file
appears in this route. Two simultaneous private-host builds with shared HB_TMP
and TMPDIR, but distinct output paths, will test whether the original failure
still exists. Do not conflate the native engine route with hb-build's separate
AOT object/link temporary files.

Measured on 1afd910c: two simultaneous tools/native-build.f processes, each
using its own private copy of host 31be3fb0, shared both HB_TMP and TMPDIR at
/tmp/alder-build-temp/shared-tmp and used distinct product-a/product-b paths.
Both exited 0 and passed the driver's startup/class smoke. The two products
are byte-identical to each other and to the host, SHA256
31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d.
The shared temporary directory is empty afterward; only each requested image
and its .names sidecar remain. Logs: /tmp/alder-build-temp/build-{a,b}.log.
The old duplicate-IMAGE-BASE failure is not reproduced on the current route.

No build code changed. docs/bootstrap.md now states the current distinct-output
rule and the capture/smoke storage facts. The original proposed per-HB_TMP lock
or child directory would guard a compiler-input file this route no longer
creates; closure is proposed on the measured replacement architecture. This
does not claim simultaneous writes to one output are safe, and macOS was not
run. No full gate, install --force, shared engine write or durable new harness.
