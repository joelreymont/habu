---
title: Drop stale paths from repair-packet dump
status: open
priority: 4
issue-type: task
created-at: "2026-10-02T09:56:33.889540+02:00"
---

Problem (review 259 of baretimeout 084eb322): tools/repair-packet-test.f DUMP-CAPTURE (:164-170) prints source:/diag:/packet: from whatever CASE-PATHS last set. TEST-NOARGS (:362-363) sets only the label and runs `--load tools/repair-packet.f --`, so a failing noargs run prints the two.* case's paths; diag: .../two.err reads as the tool's checker-jsonl.err input and misleads. The lines never help: RUN (:402-405) runs CLEANUP-RUN before T-REPORT, so the files are gone, and at EXPECT-EXIT-NZ's dump neither diag nor packet exists yet. Fix (review 259, verified on a scratch copy): DUMP-CAPTURE takes the program string last (ptr u8 n after expect) and prints `program: <it>` instead of the three path lines; :187 and :188 pass REPAIR-TOOL$ before DUMP-CAPTURE; :197 passes SRC. Acceptance: forced `99 EXPECT-EXIT` on noargs prints case: noargs, program: tools/repair-packet.f, no source:/diag:/packet: line, rc 1 with assert: expected 99 got 64; restored, `bin/hb --load tools/repair-packet-test.f` rc 0. Not baked.
