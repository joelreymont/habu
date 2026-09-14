---
title: Declare the cold checker-owner constant interface
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-14T03:06:49.235835+03:00\""
---

An empty current native image loads CHECKER-OWNER-ABI before the checker. Its public constants execute but lack pre-hook effect authority, so checked descriptor validation fails at HEADER-BYTES. Publish only the fixed numeric constants consumed by ordinary checked guard/capture code through existing PPRIM rows; preserve private and unrelated ABI-only refusal. Validate the real cold source path and retained product descriptor controls, then independent review.

The ten declared constants are HEADER-BYTES, MAGIC, BYTES, CAPTURE-OFF and the
six PAYLOAD offsets. Other native checker-owner offsets are consumed inside
its existing TRUSTED callback boundary; layout/checker aliases execute only
at top level. No dictionary-wide authority or visibility rule changes.

The empty image `/tmp/cedar-K2-native-wid-gate/hb-cold` was emitted by the
current source native writer on K2, with no retained runtime capture. Running
`--load test/checker-owner-descriptor.f` from this source passes, including a
compiled ABI-READ, malformed descriptor bounds, ABI-only refusal and private
guard-name refusal. The same image on the parent checker source refuses
VALIDATE at HEADER-BYTES with rc70. K2's retained-runtime control also passes.
Logs: `/tmp/cedar-cold-owner-abi{,-before}.{out,err}` and
`/tmp/cedar-K2-owner-abi.{out,err}`. Root owns independent review and the next
full native build/gate; these are functional source-path results.
