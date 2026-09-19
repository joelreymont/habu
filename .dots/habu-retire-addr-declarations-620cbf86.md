---
title: Retire address declarations when evaluation rewinds DATA
status: active
priority: 1
issue-type: task
created-at: "2026-09-19T20:07:59.715611+03:00"
blocks:
  - habu-make-build-fixpoint-eeaf6c00
---

A caught INCLUDE-EVALUATE of a successful storage declaration followed by throw restores DP but leaves its address row. On b4efad25, defer and PERSISTED-PTR-VARIABLE already leak; declaring typed quotation cells at definition time exposes it there too. Reuse with the opposite kind exits99 (snapshot address cell kind mismatch). Responsible boundary: EM-EVAL-THROW-RECOVER in src/habu/habu2.f and bootstrap/cg/forth.fs mirror, plus other DATA-rewind boundaries as required by the same invariant. Preserve declarations for still-live cells and remove retired rows/index entries; handle backing storage crossed by rollback. Regression: caught declaration then opposite-kind reuse, nested rollback, live prefix rows preserved, both kinds, both tiers. Probes /tmp/alder-quotation-review/{typed,defer,persisted}.f; Astra confirmed old host typed case succeeds while patched declaration case fails. This blocks landing the quotation-declaration repair for eeaf6c00. Ownership: coordinate with Hazel before engine edits; no storage wrapper workaround.

Claim: alder, .jj-ws/alder-address-rollback from c07554cf. Hazel released only EM-EVAL-THROW-RECOVER and the seed mirror for address-row retirement; no other evaluator seam, primitive or storage workaround.

Implementation: registration order is not address order (a measured late xt!
produced offsets 11205040 then 11205032). Recovery filters the existing rows
in place, retaining kind tags and relative order below the saved DP. Its row
count change makes the registrar rebuild the derived index before lookup.
If the full DATA-backed capacity crosses the rewind, recovery moves that
existing backing to mmap first. The recovery seed has no address registry
(`addr-cells-abi` is 0, xt! stores only, ptr-cell-mark drops its operand), so
the mirror states the no-op rule at the corresponding DP rewind.

The regression covers quotation/defer/persisted-pointer declarations, exact
opposite-kind address reuse, a late declaration below the rewind, nested
evaluations, and PERSIST placing the live row backing above the saved DP.
Both tiers are registered. The integrated CS host fails the row-count assert
then exits 99 on reuse; the repaired engine passes. Three private generations,
including the held quotation-declaration change e68c827d in the test tree,
are byte-identical: SHA256
`7c65af58b655cbf3e448f3d0aecafeaff676e6116dd75cd39403ae06a8683d4e`.
Check-only bootstrap and the focused product rows pass, as do the whitebox
snapshot-xt-cell-decl and native-window-owner rows. Astra's engine and fixture
reviews have no remaining findings; Hazel owns the final full gate.
