---
title: Publish the first cold checker owner after its callbacks
status: closed
priority: 2
issue-type: task
created-at: "2026-09-14T03:13:08.057517+03:00\\\"
closed-at: "2026-09-14T03:31:29.125943+03:00"
close-reason: Independent review and exact combined M focused tests pass; the campaign owns full-gate and downstream acceptance.
---

An empty current native image loads a working source checker and fills TARGET-CELL, but leaves SOURCE-CELL zero. AOT-ARM correctly refuses its missing owner. After all callbacks are installed, reuse the private claim operation only for a zero source cell; preserve nonzero retained owners and explicit TRANSFER-CHECKED. Validate real cold descriptor equality, window/capture, and replacement-owner preservation before transfer.

The checker claims after its final payload callback is installed, through a
private word. Pointer-cell relocation declarations remain unchanged. The
descriptor fixture now validates equal source/target owners and a complete
partial-window lifecycle. The real replacement-checker child asserts its old
source remains current and its new target differs before transfer.

Actual K2-emitted empty engine `/tmp/cedar-K2-native-wid-gate/hb-cold` passes
`--load test/checker-owner-descriptor.f` in this workspace; parent source fails
the added equality and refuses -8574. A separate real AOT-CAPTURE probe captures
one checked word and exactly one effect row, then executes its value 42.
K2 running `--load test/native-window-owner-child.f --
test/native-window-cast-ok.f` prints `window: 0` with empty stderr, including
the new pre-transfer ownership checks. Logs and capture probe are under
`/tmp/cedar-cold-owner-{claim,capture}*` and
`/tmp/cedar-K2-owner-claim-replace.{out,err}`. Root owns independent review,
full native rebuilding and the integrated gate; no timing claim is made.

Independent source review and combined M focused acceptance complete. M source
99caf411, SHA9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c,
built in135.602s; all eight focused suites pass. Relevant commands, exact times
and outputs: /tmp/cedar-M-focused/results.json and adjacent logs. The campaign
retains the combined full gate and downstream acceptance.
