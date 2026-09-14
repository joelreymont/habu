---
title: Serialize first registration at the address-cell owner
status: closed
priority: 1
issue-type: bug
created-at: "2026-09-14T02:44:01.149747+03:00\"
closed-at: "2026-09-14T03:31:29.120288+03:00"
close-reason: Independent review and exact combined M focused tests pass; the campaign owns full-gate and downstream acceptance.
---

Cedar. `SNAP-RELOC:EMIT-MARK` reads the row count, scans, writes one row, then
publishes the increment without synchronization. Concurrent first declarations
can overwrite one another and leave a valid-looking table missing live pointers.
This is separate from the worker DATA-base correction in 79563f22 and from the
lookup-performance followup in 3c5f6d9b.

Independent P2/source6969 reduction: four tasks each register 1024 distinct shared
cells in each of three rounds, staying below the inline capacity. A private ABI
bridge saves worker x20, supplies fixed DATA-VA only around the real existing
`ptr-cell-mark`, then restores x20. That excludes the known owner-address defect;
it is not an unmodified public-path acceptance test. Each round adds only 1024
of the expected 4096 rows; an independent membership scan finds 9216 missing of
12288 attempted distinct declarations. Adding an external acquire/release lock
around the identical primitive gives exactly 4096 rows per round and zero missing
rows. Both processes exit 0 with no stderr. Evidence:
`/home/joel/.cache/cedar-capture-rows-u5l55np1/race-bridge{,-serialized}.{f,log}`.

Proposed responsible repair: one private aligned engine mutex at shared
DATA+$1A8, outside the existing header and row backing, serializes the entire
lookup/duplicate-or-kind check/append/growth operation. The reclaimable source
heap and task-local USER storage do not own it. This unclaimed cell lies after
the MATCH family stack ($D0..$1A0 exclusive) and the seal fixture's $1A0 poke hole,
before CMFAM-CELL at $1B0. A source/recovery/application sweep found no DATA owner;
CHECKER-OWNER-ABI's $1A8 is an unrelated offset within a heap owner record.

Use the existing ARM64 acquire/release atomic convention; every successful path
releases the lock, including duplicate no-ops. Fatal refusal terminates the
process. Cold boot and post-snapshot runtime initialization reset the process
mutex, so snapshot v9, artifact v8, header schema/rows and the heap floor stay
unchanged. Count/base/capacity remain ordinary header values. Capture and source
rewind retain their quiescent-owner lifetime contract. No index is part of this
repair.

Unmodified public reproduction now confirmed on K2/source b8e069a5, SHA
`6428d167d119e683322c7f4f17b28cb6f7d10e63ffe38087df49ed355228a68f`:
`hb-integrated-K2 --load /home/joel/.cache/cedar-capture-rows-u5l55np1/race-public.f`.
No bridge or code patching is present. Expected/actual counts for the three waves
are 35460/32388, 36484/33412 and 37508/34802; membership finds 8850 missing of
12288 distinct registrations. Exact command, output and timing are in
`race-public-K2.json` in that evidence directory.

Production b85fd6a3 and the raw-emitter correction 42055449 add CASAL/STLR around
the complete owner operation and both initialization resets. The first complete
K2-hosted product built in 135.361 s, rc 0:
`/home/joel/.cache/cedar-capture-rows-u5l55np1/hb-address-lock-L2`, SHA
`7e9bae76e911b60952ba0b9b8e7b00c4cbbf497a88635ba28daa034838df81b1`.
The same unmodified public reduction now adds all 4096 rows in each wave, with
zero missing of 12288. Evidence: `lock-native-build-L2.json`, `race-public-L2.log`.

The registered `test/address-cell-tasks.f` compiles the worker subject natively,
then checks both kinds, exact baseline preservation and complete membership
through concurrent growth, duplicate no-ops and a later distinct wave. It repeats
those checks after restoring a real v9 image whose saved mutex was deliberately
set to 1. Both parent compilation tiers pass on L2. Existing 75900-row growth /
persistence / rewind and real OS allocation-refusal tests also pass at both
tiers; malformed capacity, contradictory kind and out-of-DATA declarations keep
their existing 96/99/98 refusals. The worker DATA-owner/lifecycle stress passes.
Evidence: `address-cell-tasks-L2.log`, `lock-focused-L2.json`,
`address-cell-oom-L2.log`, `image-lifecycle-tasks-L2.log`. Independent review and
the parent's combined product/full suite remain pending.

Independent source review and combined M focused acceptance complete. M source
99caf411, SHA9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c,
built in135.602s; all eight focused suites pass. Relevant commands, exact times
and outputs: /tmp/cedar-M-focused/results.json and adjacent logs. The campaign
retains the combined full gate and downstream acceptance.
