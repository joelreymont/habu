---
title: "Migrate the consumers' raw pointer cells"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.887002+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) counts consumer sites the rule habu-refuse-a-ptr-5ad2734e will refuse: maki 200 (129 in maki/src/model.f, definer-generated accessors that habu-fix-the-definers may remove), Tender 11, loom 10, kiba 6, radar 0; the rule lands in the engine only when every consumer loads on it. Acceptance: each consumer's owner receives its census rows with the declared forms to use (PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, TYPED-BUFFER) and the read-only rule engine ~/.cache/hazel/engines/raw-rule-gen1 to verify against; each consumer's suites load on that engine without E-RAW-CELL-PTR before the rule lands; the maki count re-measured after the definer fix. Files: maki, Tender, loom, kiba sources per the census. Verify: each consumer's suite on raw-rule-gen1. Depends: habu-fix-the-definers; the rule lands after this. Ownership: hazel announces, each consumer's owner converts. Parent: habu-refuse-a-ptr-5ad2734e. Claim: agent=hazel (notices).
