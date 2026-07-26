---
title: Seal model provenance pins
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:51:04.946739+02:00"
blocks:
  - habu-publish-make-only-a40591e2
---

Problem: MODELPROV:mprov exposes public UNMAKE, so a holder of one genuine pin can reuse prov-proof around arbitrary decoded key cells and forge a stored manifest identity. Required result: declare mprov with DESTRUCT owner, migrate original-block inverse uses to destructure mprov, retain PIN as the sole proof mint, and let ;package seal MODELPROV. Retire only the obsolete UNMAKE forgery caveats. Do not claim cryptographic authenticity, change digest/preimage semantics, add a text inverse, or couple provenance to compute identity. The existing MODELPROV-TEST package remains black-box; no reopen or test friend is permitted. Owner: maki/infer/model-provenance.f and its existing suite only. Dependency: habu-publish-make-only-a40591e2. Acceptance: the existing 25-line external UNMAKE/re-MAKE inverse and a package-reopen variant flip from ACCEPT to checker rejection through the real module load; PIN, KEY-HEX, equality, digest mismatch, and domain-separation snapshots remain unchanged; no public projection exposes prov-proof; model-provenance, package, typed-local, signature, and trust gates pass.
