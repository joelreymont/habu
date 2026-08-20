---
title: "Route 3: the type foundation loads post-hook, checked"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:53:19.909085+02:00"
---

The zero-trust route for the 485 recording-gap TRUSTED: sites (fab55650's blocked set, epic 4fd12d60): src/habu/habu2.f:861-890 loads type-schema.f, type-family.f, sumtype.f and checker.f BEFORE src/core/check-hook.f installs the hook, so their : definitions record no signature - measured 2026-08-19 by trusted-1 through the real prefix build (ED-PROBE rc 70 evidence on the leaf of fab55650). Move the type foundation post-hook so its signatures are DERIVED AND CHECKED - the bootstrapping knot is the checker checking its own foundations, which is the same territory as habu-seal-the-checker-5314c0ab (package ownership of checker.f) and the dissolved umbrella habu-tfam-2b-sealed-1b77662c (re-derive before dispatch). Rejected alternatives, with reasons on fab55650's leaf: owner-side declared-signature recording (trust that route 3 would delete), mass PRIM axioms (contradicts the epic by its own text). This blocks fab55650's remaining 485 sites and therefore feac682b (the reader deletion).

SCOUTED VERDICT (2026-08-20, full map in the scout report; re-measured):
1. THE KNOT IS ONE EDGE, NOT FOUR FILES: check-hook.f's closure is checker.f
   alone (0 hits into the 825 type-file names). The only real obstruction is
   render.f -> type-family (13 token sites, 5 load-bearing in REND-SIG's path)
   - render.f must stay pre-hook because it installs RECXT, the ONLY
   inferred-effect row producer, whose default silently discards signatures.
   Cut with the existing checker defer wall (checker.f:465-482/:896-907,
   bound at type-family.f:3218-3239) - proven in-tree instrument.
2. SCOPE: move type-schema.f, type-family.f, sumtype.f, layout-buffer.f,
   layout-valid.f AS ONE BLOCK to immediately after the hook row (zero-length
   window; the last two have zero pre-hook consumers and cost nothing).
   checker.f CAN NEVER MOVE: its 129 forwarder sites (70 distinct words) are
   conceded to PPRIM: axioms - the tree's own stated preference
   (prefix-rewind.f:41-43, LESSONS.md:204-209). Route 3 unblocks 352 sites,
   not 485; the ruling says so.
3. REPLAY-AT-HOOK IS STRICTLY DOMINATED: it still requires the 662 definitions
   to typecheck and adds either route-1's declared-text carrier (rejected) or
   a second CHECK pass over the same bytes (second authority).
4. PRECEDENT PRICED: structures.f moved across the hook 2026-07-15
   (ksxvzllqmnks) - 14 files, 11 ordered manifests, the exact churn list is in
   the scout report.
5. HARD GATE BEFORE ANY MANIFEST EDIT: run tools/check-core.f over the three
   files and COUNT THE REJECTS. 662 definitions have never met the checker.
   Encouraging: near-zero raw-pointer surface in schema/family. Discouraging:
   their post-hook peers run 35/44 and 39/55 TRUSTED:-to-definition ratios.
   If the owner-side bill approaches that, route 3 nets far less than the
   epic assumes - re-open the ruling on that number.
