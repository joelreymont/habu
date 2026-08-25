---
title: Resolve generated constructors in preverify
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:43.399147+02:00"
---

Problem: the check-tool preverify nominal pass cannot resolve declaration-GENERATED constructor words. Reproduced: ARTIFACT-ID--RESULT:OK and MAKI-DOMODE:TRAIN fail E-UNDEFINED, byte-identical on the clean parent - an older wall that maki/db/promotion.f now hits because the replay registration entry (7e5f3c06ccf4) clears declaration registration for it. Cause: preverify resolves words against source-visible definitions; constructors exist only after generation, but the registration entry stamps constructor packages, so the information preverify needs is in the stamped rows. Required result: teach the preverify nominal pass (tools/check-core.f, CHK-PREVERIFY-* around line 1259) to resolve FAMILY:VARIANT constructor names from the stamped registration rows instead of treating them as undefined. Acceptance: bin/hb --load tools/check.f -- maki/db/promotion.f passes preverify; a fixture with a misspelled variant name still fails E-UNDEFINED; a mutation dropping the stamped-row lookup fails the fixture. Files: tools/check-core.f and its focused test. Verify: the check tool suite plus the promotion.f reproduction. Depends: none. Ownership: preverify generated-word resolution only. Claim: unassigned.
