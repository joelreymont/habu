---
title: Preserve foreign symbol cleanup across image captures
status: active
priority: 1
issue-type: task
created-at: "2026-09-20T01:51:57.773048+03:00"
---

FUNCTION: bindings cache process-owned addresses. IMAGE-LIFECYCLE:PREPARE removes
the forget hook, but FN-REGISTERED stays set. Measured getpid call -> PREPARE ->
call -> PREPARE hook counts 1,0,0,0; the second restored process-image capture
dies with SIGSEGV when the deadline waitpid binding is present. Claim: alder,
.jj-ws/alder-capture-image. Re-land deadline dot 2368e5a6 separately on top.

Register the forget hook once at declaration through REGISTER-PERSISTENT from
5f87aeed. FN-RESOLVE stays unchanged, so stripped calls do not reach the image
lifecycle. The earlier runtime re-registration repair fixed process-image but
pulled the unsupported runtime xt! registrar into stripped closures; it is
superseded. The repeated call/prepare fixture must keep one hook throughout,
and repeated restored process images prove that resolved addresses are forgotten.
No full gate or shared engine write; Hazel owns integration and closure.

Final proof: ffi-cabi and process-image pass, and the stripped getpid program
builds and prints 1 (exit 0). The final three-commit tree passes all 25 focused
owning/reader rows, including the full hb-build fixture row, on private gen3.
All three native generations have SHA-256 31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d.
Astra review is clear; artifacts are in /tmp/alder-capture-persistent.
