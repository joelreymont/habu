---
title: "Hashmap: fail closed on full table and invalid capacity"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T14:05:23.103328+02:00\""
---

Problem: lib/hashmap.f:26-32 HM:PROBE linear probe has no slot counter — full table + absent key loops forever (demonstrated: cap 4, 4 inserts, probe key 99 -> killed by timeout). lib/hashmap.f:23-24 mask arithmetic assumes power-of-two cap: cap=0 makes mask identity, returning slot=hash (demonstrated OOB slot 12345 against 1-cell arrays -> wild read and caller writes keys[slot]); non-power-of-two probes a subset. Expected fix: structural invariants, not value heuristics — validate cap at construction/entry (power-of-two, nonzero) with named throw (E-HM-CAP), and bound the probe loop at cap steps with named throw (E-HM-FULL) on wrap. Acceptance: T{ }T negatives: full-table probe of absent key -> E-HM-FULL (no hang); cap 0 and cap 3 -> E-HM-CAP; existing suite stays green. Files: lib/hashmap.f, lib/hashmap-test.f, lib/errors.f (named codes). Verify: bin/hb --load lib/hashmap-test.f; maki/test.f slice touching hashmap consumers. Depends: none. Ownership: lib/hashmap.f. Claim: agent=claude workspace=.jj-ws/habu-hashmap-fail-closed-82251e59.
