---
title: Add pointer admissibility controls
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.180087+02:00"
blocks:
  - habu-add-ptr-identity-8f4bb961
  - habu-fix-payload-suite-2a65b9aa
---

Why: codex blind review of landed 6b10df82 and c366d60d - TC21-TC28 and the payload-suite negatives never prove the pointer signatures themselves are admissible; a blanket rejection of ptr-of-generic-linear rows would keep every negative green, so the pins do not yet discriminate transport rejection from type inadmissibility. Also three fixture pairs (constructor-branch, MATCH keep, MATCH exit) have non-linear twins that ALSO reject - generic control or arity failure satisfies them, so their linearity labels overclaim, and the FILEMAP paired-refusal claim is wrong for them. Behavior: accepted identity controls added beside the negatives - ( ptr opt<linear> -- ptr opt<linear> ) and the two-layer form certify, plus ( ptr held -- ptr held ) in the payload suite; the three mislabeled pairs get discriminating controls or explicit NOT-LINEARITY banners per the suite's own established discipline; FILEMAP text corrected to claim exactly what is paired. Owner: test/type-linear-suite.f, maki/infer/gpt2-payload-test.f, FILEMAP.md. Acceptance: the identity controls certify on master; re-applying the m14 mutation still kills exactly the four transport negatives while the new controls stay green; suites rc=0; diff lints clean. Real pre-change defect: a hypothetical blanket ptr-row rejection passes today's entire negative set.


Reshaped 2026-07-26 (codex gate-stop accepted): coordination parent; implementation in habu-add-ptr-identity-8f4bb961 (type-linear-suite controls) and habu-fix-payload-suite-2a65b9aa (payload suite + FILEMAP corrections), independent leaves.
