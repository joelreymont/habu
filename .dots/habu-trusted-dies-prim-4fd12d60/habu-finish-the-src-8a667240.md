---
title: Finish the source-owned native runtime build
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.327440+03:00"
---

Owner: Cedar; integration workspace .jj-ws/cedar-image. Complete the existing full-runtime native capture and dynamic compiler storage changes, keeping runtime sources independent of the discarded build host. Preserve current tested work at tysuuumo/e9fd7e6d and preceding cedar-runtime/cedar-calls/cedar-fixes revisions; use a new child for edits so agent bases stay fixed. Cold build5 passes; warm self-build4 fails compiling NATIVE-BUILD:LOGICAL-RESET at seed-ndict! with E-NELAB-CALL (-8286), log /tmp/cedar-self-build-4.out. Resolve engine-internal primitive lookup through the native compiler without restoring the legacy compiler. Acceptance: rebuilt bin/hb contains the full checker/compiler/REPL, rebuilds itself through tools/native-build.f, and passes focused large-definition, IR-context growth/release, and generic-call tests. Source ownership: tools/native-build.f, src/habu/native-runtime.f, compiler storage/capture and dictionary lookup; coordinate overlaps with quotation and app-image dots.
