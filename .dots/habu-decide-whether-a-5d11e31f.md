---
title: Decide whether a raw store may launder a stale cell
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T15:53:55.695142+03:00"
---

The catch-stale rule (89902bde) refuses every TYPED use of a cell a caught throw left stale (E-STALE-READ), but FENCE-WHY (src/core/checker.f:2345-2356) tests the tag against var, family, con, pointer and quotation and admits everything else, so a stale cell passes a raw store: ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v V ! V @ 1 + certifies over a variable V and a throwing WBOOM ( ptr u8 -- ), and the fetch hands back a plain raw cell. Measured on the lane engine 311a70d1 (probe in the catch-stale lane scratch, w6/probe-raw-launder.f). This is a language decision: either the fence refuses a stale cell (the store is a read of a cell with no type left, named E-STALE-READ; migrate the sites the tree has) or the admission is the rule and forth.md says a raw store is the escape and why. Depends: 89902bde. Ownership: hazel; the decision is Joel's. Claim: unassigned.
