---
title: Attribute the native public preseed helper's tag diagnostic
status: open
priority: 2
issue-type: bug
created-at: "2026-09-14T12:58:00Z"
---

Focused follow-up to habu-green-the-aot-d30c5a39. FETCH-RUN-BAD in test/gate-aot-positive-lib.f expects exit 85 and "hb: bad layout tag" from its public AOT-LAYOUT-FETCH-BAD:HLP preseed helper. The current optimizing engine produces exit 85 and "hb: bad res tag" for the same helper and forged tag. The expected diagnostic has not been weakened or changed.

Same-engine attribution: /tmp/cedar-data-site-native SHA-256 515efca813407dc1992614a87e1103f5e07670546f21e30139bb65f39aa3db24 was both driver and producer (HABU_FIXPOINT_ENGINE explicitly set); bin/hb also pointed to it for lint children. Frozen baseline 1948ba71 with the old bare HLP selector and the entry-lookup fix with qualified AOT-LAYOUT-FETCH-BAD:HLP both built successfully. Both fresh images exited 85 with identical "hb: bad res tag" stderr. Their complete binaries were byte-identical, SHA-256 196d55a254441144f3ff0255521c8f0ce6582f0c2f4ba25d788b0b5f2e9e4f44. Therefore the diagnostic mismatch predates and is independent of the entry-identity fix.

Source: /tmp/cedar-entry-preseed-fetch.f, copied from PRESEED-FETCH-SRC; seed 00000000000000000000000000000005. Evidence: /tmp/cedar-entry-preseed-fetch-baseline-{build,run}.log and /tmp/cedar-entry-preseed-fetch-{build,run}.log. The fixture stores the forged res<n,n>, fetches it and matches it; determine which operation the native compiler rejects first before correcting the diagnostic assertion or revising the case to reach its intended fetch path. Do not claim the complete legacy AOT-positive suite passes from this bounded check.
