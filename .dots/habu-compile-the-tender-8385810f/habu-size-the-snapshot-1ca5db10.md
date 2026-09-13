---
title: Size the address table for the complete Tender closure
status: active
priority: 1
issue-type: task
created-at: "2026-09-12T10:41:22.834193+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Claim: Cedar.

Own layout.f XTCELL/AOT capacity, habu2.f capacity refusal and fixture only. Rebase reviewed pending0d78b97f; it is not integrated. Measure runner-inclusive closure/headroom before65536 bound; old19088 comment is stale. Verify40000 declarations exceed old32768, exact capacity refusal count/cap/newline, first-generation actual versus advertised limit and complete Tender build. Corrected native layout is prerequisite; ancient seed refresh is not. Registrar index follows this layout contract.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

2026-09-13: current tracked B2 starts with 30885 rows (30887 after APP-IMAGE
support loads); real SAVE exits96 at32768. Ported the cap/encoding fix without
the obsolete second-generation workaround:65536 rows, full-width cap loads,
one contiguous diagnostic including the cap and newline, and section reservation
increased by32768*8 bytes. Relocation proof/source and assembler suites pass.
New real-load tests require40000 total rows and exact advertised capacity,
including duplicate registration at the bound. First rebuilt product, complete
capture/Tender closure measurements and full gate are still pending.

Tracked B3 source 0c1ca1f3 verifies the real first-generation 65,536-row bound:
40,000 rows pass, duplicate registration at capacity passes, and the next row
refuses rc 96 with exact count and newline. Full-gate app-image and process-image
still hit that bound. Their logged `got 46` was the stderr-length assertion, not
the child exit code. Attribute which stage adds the rows and distinguish live
closure needs from stale/duplicate registrations before changing storage again.

Product E (a99cd3fc) resolves the repeated-capture overflow in app-image and
process-image: the live SYMS allocation had moved from its boot buffer to a grown
mapping, and each SAVE then recopied even its already persisted DATA replacement.
REG-PERSIST-MOVE now retains complete allocated DATA spans. Both image suites
pass on E, with the separate all-native startup fixture correction for app-image.
Tender 4cc58705 currently stops earlier at native OPEN/-8650; its complete captured
closure/headroom is still unmeasured, so this leaf stays active.

G2 source 292f9cdb now compiles past Tender OPEN, but the complete standalone
capture on frozen Tender 4cc58705 still exits 96 at 65,536 rows. The run took
64 seconds under concurrent work and produced no executable; log
/tmp/cedar-tender-G2/build.log. Distinguish genuinely live growth from obsolete
registrations before extending storage. The earlier E image-suite repair stays
valid; complete Tender closure/headroom is still unmeasured.

2026-09-14: the first Tender capture needs 75,900 unique rows on G2. A seven-symbol
probe measured 31,230 existing rows plus 44,684 current-symbol fields. Of the old
rows, 30,886 belong to SYMS-BOOT, which remains a supported view; retiring that
range would lose valid relocation facts. Repeated persistence retains the same
live DATA span. This is genuine closure growth, not repeated copying.

Production 6969dc7a replaces the fixed limit with an ordered growable vector in
the existing reserved band. Geometric mmap growth preserves the old storage on
failure; capture moves its complete capacity into DATA, and source rewind first
detaches any backing span that crosses the cut. The immutable `addr-cells-abi`
primitive selects the schema before mutable header admission. Native snapshots
use strict v9 headers; actual source-host captures on legacy G2 remain v8 and
restore. Artifact rows remain v8/eight-byte values, with dynamic staging and an
actual aggregate section budget. The heap floor is unchanged. Registrar lookup
cost remains open in 3c5f6d9b.

First G2-hosted native product P2 built in 143.102 s, rc 0, SHA
`ef4a7aa34ada381c90435f98b10298aad4a9030e674cdc5cdc27ec103538ff4c`.
Frozen Tender 4cc58705 then builds through its public `scripts/habu.py build`
entry in 63.822 s, rc 0. Its v9 executable has 76,154 unique rows and capacity
131,064. Two public REPL recaptures retain every row in the same order and the
same DATA-relative backing base; the third image restores and runs its REPL.
File/DATA size convergence is not claimed. Provenance and exact row comparison:
`/home/joel/.cache/cedar-capture-rows-u5l55np1/tender-P2/recapture-headers.json`.

Focused P2 acceptance covers 75,900 real registrations, every row across growth,
duplicates, persistence and complete/straddling source cuts, actual OS allocation
refusal, overflow refusal, grown-snapshot owner reset at both tiers, nine malformed
v9 header refusals, and actual artifact WRITE/READ/OWN/IMPORT/MERGE with aggregate
budget refusals at both tiers. Existing image, relocation, declared-XT and native
defer controls pass. Integrated rebuild/full gate and independent final test
review remain the landing gate; the leaf stays active until that integration.
