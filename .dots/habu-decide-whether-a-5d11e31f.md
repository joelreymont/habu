---
title: Refuse a stale cell at a raw store
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T15:53:55.695142+03:00"
---

The catch-stale rule (89902bde) refuses every typed use of a cell a caught throw left stale (E-STALE-READ), but FENCE-WHY (src/core/checker.f:2345-2356) tests the tag against var, family, con, pointer and quotation and admits everything else, so a stale cell passes a raw store: ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v V ! V @ 1 + certifies over a variable V and a throwing WBOOM ( ptr u8 -- ), and the fetch hands back a plain raw cell. Measured on the lane engine 311a70d1 (probe in the catch-stale lane scratch, w6/probe-raw-launder.f). A raw store reads its source cell and must refuse a stale value with the existing E-STALE-READ; it cannot clear the stale state by storing through raw memory. Acceptance: the laundering program is rejected at the store, the existing typed catch-stale refusals remain, unaffected raw stores still compile and run, and any in-tree sites that relied on the escape are migrated. Test through the real load path with the failed laundering case and positive unaffected stores; rebuild and run the full native suite for shared checker behavior. Depends: 89902bde. Ownership: checker. Claim: unassigned. Queue after current incremental critical work.
