---
title: "HM: validated table handle constructor"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:26:18.667993+02:00"
---

Problem: lib/hashmap.f has no constructor — cap and caller-owned keys[]/used[] arrays are passed per call, so the valid-table invariant (power-of-two cap, arrays sized to cap) has no single structural owner; cap is re-validated at every PROBE/CLEAR entry (correct but per-call). Expected: HM:NEW ( keys used cap -- table ) validating once (E-HM-CAP) and binding cap+arrays into a handle; PROBE/CLEAR take the handle; migrate the three consumers (lib/hashmap-test.f, maki/examples/nanogpt/bpe.f, bpe-full.f). Follow-up to habu-hashmap-fail-closed-82251e59; interface change, sequence after that dot lands. Acceptance: handle API with T{ }T coverage incl. negative construction; consumers migrated; per-call validation removed only where the handle proves it. Files: lib/hashmap.f, lib/hashmap-test.f, maki/examples/nanogpt/bpe.f, bpe-full.f. Verify: bin/hb --load lib/hashmap-test.f + bpe suites + maki/test.f. Depends: habu-hashmap-fail-closed-82251e59. Ownership: HM module API. Claim: unassigned.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-hashmap-fail-closed-82251e59
is no longer in the graph: it was closed and archived by commit 75f1d0a5d "Close landed
hashmap and fmt dots", so the fail-closed hashmap this interface change sequences after is
landed and the dependency is satisfied. Nothing blocks this dot now.
