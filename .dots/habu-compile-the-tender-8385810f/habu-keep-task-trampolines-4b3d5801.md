---
title: Keep task trampolines out of retained compiler code
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T04:33:02.496464+03:00"
---

Actual current M host-io-image executes concurrent UDP/serial I/O and cleanup/reuse twice, then capture refuses rc100: retained code lacks native provenance. The fixture local-buffer shadow is separately repaired in daeb3424 and explicit native subject compilation does not cure this refusal. lib/task.f TASK-ENTRY-BUILD writes a 92-byte pthread ABI adapter directly at cp@ using patch32, advances cp!, and RESET-SYMBOLS only clears TASK-ENTRY. Temporary process code remains in the retained compiler region without provenance. Give the task adapter the correct native owner/lifetime using existing facilities; do not certify unknown code or bypass snapshot checks. Prove the exact gap, native task capture across fresh generations, cleanup/reuse and concurrency; preserve JIT-load behavior. Cedar owns reduction/design. Evidence /tmp/cedar-M-host-io/native-subject.{json,log}.

2026-09-14: replaced the runtime emitter with an immutable engine entry and a
TRUSTED-only C-address getter. Shared TASK-ABI offsets are checked against the
library's typed fields. Native candidate `/tmp/cedar-task-entry/hb-task-entry`
(SHA-256 `ed0b39ce7c4f7a3ec34cab43f0dd7f5b2c8f60aa7b03d02f3e53d4071120d5ba`)
built in 136.948 s. The real host-I/O image test passes all three generations,
concurrent UDP/serial, quiescence and reuse in 8.475 s. Task-entry, full task
library and clobber suites pass. The old M control fails the code-space
assertions by exactly 92 bytes per restart. Final explicit frame-register
emission was independently compared with the candidate's cold engine and is
byte-identical; typed layout assertions also pass through the real load path.
Evidence: `/tmp/cedar-task-entry/{focused,task-controls,native-host-io,final-source-controls}.json`.
Independent Astra review approved the source and independently passed the
task-entry test, plus direct and quotation-based raw-entry refusals in both
tiers. The combined native gate remains before closure.
