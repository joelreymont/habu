---
title: Keep task trampolines out of retained compiler code
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T04:33:02.496464+03:00"
---

Actual current M host-io-image executes concurrent UDP/serial I/O and cleanup/reuse twice, then capture refuses rc100: retained code lacks native provenance. The fixture local-buffer shadow is separately repaired in daeb3424 and explicit native subject compilation does not cure this refusal. lib/task.f TASK-ENTRY-BUILD writes a 92-byte pthread ABI adapter directly at cp@ using patch32, advances cp!, and RESET-SYMBOLS only clears TASK-ENTRY. Temporary process code remains in the retained compiler region without provenance. Give the task adapter the correct native owner/lifetime using existing facilities; do not certify unknown code or bypass snapshot checks. Prove the exact gap, native task capture across fresh generations, cleanup/reuse and concurrency; preserve JIT-load behavior. Cedar owns reduction/design. Evidence /tmp/cedar-M-host-io/native-subject.{json,log}.
