---
title: The pre-pass follows every word a load creates
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:56:55.869392+02:00"
---

Problem: carl-hcdd's fix (zkmnmrls f455cb19, dot 6dd65911) taught src/habu/verify-source.f that a create-less does> word patches the last created word (LAST-CREATED, CREATED!, PATCH-LAST), but LAST-CREATED moves only for the creating forms the pre-pass models: (1) an unmodelled top-level call that runs create leaves it on the older word, so a later create-less does> call patches, in the pre-pass's model, a word the load does not touch (the symbol check catches only a forgotten word); (2) a resident (engine-provided) create-less does> word is still recorded as a definer, not a patch; (3) begin-structure fields create words the pre-pass does not count as created. Each lets check and --verify-only disagree with --load about the patched word's type. Acceptance: for each of the three, a file where a create-less does> patch follows the form gives the same verdict and, for a refusal, the same packet under --load, plain check and --verify-only. Files: src/habu/verify-source.f (VERIFY-DOES, CREATED!, the top-level call path), test/certify-does-definer.f section 17. Verify: three probes, one per form, in each mode. Depends: carl-hcdd fix lands. Ownership: verify-source.f LAST-CREATED. Claim: unassigned.
