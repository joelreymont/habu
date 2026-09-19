---
title: Honor capture deadlines after output EOF
status: active
priority: 2
issue-type: task
created-at: "2026-09-20T01:12:36.013155+03:00"
---

Astra review of 05728727 measured a child closing stdout/stderr then waiting one second: RUN-ARGV-ENV-CWD-STDIN-CAPTURE returned after 1023 ms under a 100 ms deadline. PROC-RUN-*-CAPTURE-LOOP stops on stream EOF and PROC-CAPTURE-FINISH-RC / outcome loops then use blocking PROC-REAP-CAPTURE. Fix the shared process capture lifetime at lib/process.f, including rc and outcome paths, with regressions for early EOF plus a live child and ordinary completed children. No builder workaround; candidate boot check depends on this.

Claim: alder, .jj-ws/alder-capture-image. Hazel released lib/process.f for one
shared WNOHANG reaping loop and the existing deadline and kill-and-wait timeout;
fixtures in lib/process-test.f and lib/process-cwd-test.f. Use TASK:SLEEP capped
by the remaining deadline: Astra measured TASK:PAUSE letting TASK:HALT exit the
captor with its child still alive. Hazel accepted the non-cancelling sleep.

The first landing was withdrawn after DB exposed process-image assert 9:
the second restored application returned 139. The new waitpid binding exposed
the FFI cache's missing cleanup registration after the first capture, measured
and fixed separately by habu-re-register-foreign-9c283543. This re-landing
keeps the deadline implementation and fixtures unchanged, on top of that fix.
The private DB2 engine reproduces assert 9 before the FFI fix. On the final
persistent-hook/FFI/deadline stack, private gen3 passes process-image and all
25 focused owning/reader rows, including hb-build-fixtures. All three native
generations are identical at 31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d;
artifacts are in /tmp/alder-capture-persistent. Astra review is clear. The full
gate and the build-fixpoint fixture that invokes install --force remain with Hazel.
