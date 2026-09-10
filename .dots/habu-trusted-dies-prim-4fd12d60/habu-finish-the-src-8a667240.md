---
title: Finish the source-owned native runtime build
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.327440+03:00\""
---

Owner: Cedar; integration workspace .jj-ws/cedar-image. Complete the existing full-runtime native capture and dynamic compiler storage changes, keeping runtime sources independent of the discarded build host. Preserve current tested work at tysuuumo/e9fd7e6d and preceding cedar-runtime/cedar-calls/cedar-fixes revisions; use a new child for edits so agent bases stay fixed. Cold build5 passes; warm self-build4 fails compiling NATIVE-BUILD:LOGICAL-RESET at seed-ndict! with E-NELAB-CALL (-8286), log /tmp/cedar-self-build-4.out. Resolve engine-internal primitive lookup through the native compiler without restoring the legacy compiler. Acceptance: rebuilt bin/hb contains the full checker/compiler/REPL, rebuilds itself through tools/native-build.f, and passes focused large-definition, IR-context growth/release, and generic-call tests. Source ownership: tools/native-build.f, src/habu/native-runtime.f, compiler storage/capture and dictionary lookup; coordinate overlaps with quotation and app-image dots.


Internal lookup is fixed and independently reviewed in 205d7b00, above nominal-pointer fix 3cde4f9d. Cold build6 and both focused regressions pass; warm self-build5 is running from frozen source (/tmp/cedar-self-build-5.out). In-process peer handoff: .jj-ws/cedar-handoff, parent205d7b00, bin/hb SHA256 5463d3842d686dc45eb844867dfd6130b13f415e0bae28b46388972b1e3f1c4c. Standalone acceptance and full suite remain pending.

Warm self-build5 passed the original internal-call blocker, then failed compiling REG-PROTECT because REG-PROT-N was not registered after LOGICAL-RESET disabled the defining hook. The old native checker still checks ordinary bodies. Reduce reset/load-util and fix metadata staging before another full warm rebuild; app_image owns this after its app regression completes. Log: /tmp/cedar-self-build-5.out.
