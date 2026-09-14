---
title: Keep application recapture extents bounded
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-14T03:00:17.175483+03:00\""
---

Plan: PLAN.md. Root owns attribution and integration; implementation remains unassigned. Actual P2 app capture on frozen Tender 4cc58705 preserves all 76,154 unique address rows through two recaptures, but file sizes are 30,539,968 / 58,654,912 / 90,046,656 bytes and DATA sizes are 18,003,568 / 21,280,368 / 24,557,168 bytes. DATA grows exactly 3,276,800 bytes per recapture with no new source definitions or rows. Product SHA ef4a7aa34ada381c90435f98b10298aad4a9030e674cdc5cdc27ec103538ff4c; evidence /home/joel/.cache/cedar-capture-rows-u5l55np1/tender-P2/recapture-headers.json. This is distinct from the closed native selfbuild growth b2f3c39f. Trace snapshot text-prefix extent and capture-owner persistence; fix each proven owner without dropping live dictionary, checker, application data or declared address rows. Preserve an existing snapshot's immutable engine prefix instead of nesting previous payload bytes, if confirmed. Reuse existing persistent storage when unchanged, if confirmed. Acceptance: repeated fresh-process APP-IMAGE:SAVE with no new definitions reaches bounded stable text/DATA extents, keeps exact row membership, restores application behavior and checked REPL, and still admits legitimate later data growth. Rebuild and run the existing native gate after the responsible changes. No byte-fixpoint claim until same-source contents also compare; existing 8d249e4d owns build identity.


Confirmed attribution: SNAP:HDR takes its entire current image text extent as
the next immutable prefix. Actual derived prefix sizes are 5,697,536 /
30,535,680 / 58,650,624 bytes. USIGS-SNAPSHOT-PERSIST and
NORET-SNAPSHOT-PERSIST unconditionally allocate their existing capacities:
2,949,120 + 327,680 = 3,276,800 bytes. A direct public Tender REPL probe
confirms those copies with unchanged store contents. Evidence:
/tmp/cedar-recapture-attribution/{extent-headers,store-probe}.json.

The patch derives the current snapshot's prefix from its already validated
trailer and reuses complete effect/control allocations already in live DATA.
Growth scratch resets on both paths. It does not scan for or reclaim older
nested payloads in historically bloated input images; it stops further nesting.
The new real-store regression fails eight identity/heap-stability assertions on
K2 while its full-content comparisons pass. The writer loads through the
optimizer; the extended application fixture compiles. Rebuilt-product, repeated
fresh-process capture and independent source review remain pending.
