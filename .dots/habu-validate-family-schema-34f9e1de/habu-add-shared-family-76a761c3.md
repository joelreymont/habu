---
title: Add shared family pointer query
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.304435+02:00"
---

The shared authority for the incomplete-family pointer soundness hole. One pure query owned by package FAMILY-SCHEMA in src/core/type-family.f with the exact public effect BAD-PTR ( fam -- ptr u8 n bool ): scans the completed provisional TYPE-FIELD range by exact family id, recursively covers nested SC-PTR and SC-APP arguments, follows final applied-family ownership, terminates on the expressible self-pointer cycle, and returns the first offending field name with true, or false. No declarer integration in this leaf. Fixture: production-built - declare the currently accepted slaunder structure and elaunder enum reproductions, prove the query names p; safe non-linear self-pointers and same-tail families in different packages return false. Acceptance: query suite green; both diff lints; TRUSTED.md row for any forwarder in the same commit.

Claim: agent=valfam workspace=.jj-ws/habu-validate-family-schema (three-commit lane covering this leaf then the two integration leaves per the reshape).

Amended 2026-07-26 (checkpoint finding accepted, option 1): the single authority TFAM-CONCRETE-LINEAR? is asymmetric on in-progress families - the product branch walks provisional PF-ROW rows while the sum branch reads through the committed watermark (SUMV-PAY-N raises E-TFAM-PAYLOAD via PF-COMMIT-N, type-family.f:1663-1669 vs the sum path) - so the ENUM close cannot ask it the question. This leaf therefore ALSO makes the authority symmetric in the same commit: the sum branch walks the family's own field rows by exact family id the way the product branch does, retaining the legacy SUMV-SCH walk for families the unified declarers did not create. Behavior-identity proof required for committed unified sums (measured pre/post equality on existing families). Integrity-coverage proof required: the E-TFAM-PAYLOAD integrity throws that no longer fire from the linearity walk must be shown to still fire on the paths that own payload-metadata integrity, via a fixture with malformed metadata rejecting through a real owner - the linearity walk was never the right integrity checkpoint, but its coverage does not get silently retired. Measured leaf-text corrections from the checkpoint: mutual two-family cycles are NOT expressible (forward reference rejects 7109 - state the measurement, no fixture inventable); direct self-containment is refused before the walk (7127/7133 - why no visited set is needed); the ENUM reproduction uses a multi-character variant name (single-character names are reserved, 7110) with the offending FIELD still p.

Amended (codex preflight 5): required negative - an internal-word production fixture proving ordinary user source cannot call FAMILY-SCHEMA:BAD-PTR (a TRUSTED.md row is bookkeeping, not confinement).
