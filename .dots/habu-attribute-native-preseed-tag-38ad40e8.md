---
title: Validate typed memory loads before native code returns a layout value
status: open
priority: 1
issue-type: bug
created-at: "2026-09-14T12:58:00Z"
---

Focused follow-up to habu-green-the-aot-d30c5a39. FETCH-RUN-BAD in test/gate-aot-positive-lib.f expects exit 85 and "hb: bad layout tag" from its public AOT-LAYOUT-FETCH-BAD:HLP preseed helper. The current optimizing engine produces exit 85 and "hb: bad res tag" for the same helper and forged tag. The expected diagnostic has not been weakened or changed.

Same-engine attribution: /tmp/cedar-data-site-native SHA-256 515efca813407dc1992614a87e1103f5e07670546f21e30139bb65f39aa3db24 was both driver and producer (HABU_FIXPOINT_ENGINE explicitly set); bin/hb also pointed to it for lint children. Frozen baseline 1948ba71 with the old bare HLP selector and the entry-lookup fix with qualified AOT-LAYOUT-FETCH-BAD:HLP both built successfully. Both fresh images exited 85 with identical "hb: bad res tag" stderr. Their complete binaries were byte-identical, SHA-256 196d55a254441144f3ff0255521c8f0ce6582f0c2f4ba25d788b0b5f2e9e4f44. Therefore the diagnostic mismatch predates and is independent of the entry-identity fix.

Source: /tmp/cedar-entry-preseed-fetch.f, copied from PRESEED-FETCH-SRC; seed 00000000000000000000000000000005. Evidence: /tmp/cedar-entry-preseed-fetch-baseline-{build,run}.log and /tmp/cedar-entry-preseed-fetch-{build,run}.log. The fixture stores the forged res<n,n>, fetches it and matches it. Do not change its expected fetch diagnostic.

Confirmed 2026-09-14 on combined engine SHA-256
8c1b07555a940b6ecf0ea1a42566631ef68aaef1a1200ae23e2d747a8d1fd39c:
removing MATCH exposes the missing check. The checked preseed entry in
/tmp/cedar-typed-fetch-native-subject.f stores its two-cell sum value into an
allocated LAYOUT-BUFFER, fetches it, drops it, and prints a marker. Building via
hb-build with --preseed-entry CEDAR-FETCH-NATIVE:HLP and seed
000000000000002a0000000000000005 succeeds. The fresh image exits 0 and prints
"typed fetch returned" despite tag 5 being outside its two-variant domain.
All memory accesses use the allocated buffer. The corresponding JIT fetch
exits 85 with "hb: bad layout tag". Logs are
/tmp/cedar-typed-fetch-{native-build,native-run,jit}.log.

Owner: Cedar. NELAB:WIDE-LOAD emits scalar LOAD operations and bundle glue with
no validation; the scalar-width branch also needs consideration for enums.
The JIT consumes the source-bound LOWER-CERT fetch descriptor and validates
active nested tag domains before loading. Native lowering must preserve that
same contract using the correct declaration owner's frozen facts, including
one-cell sums/enums, nested products/sums and inactive variant padding. Keep
validation ordered before returning a typed value and preserve it when the
result is dropped. Add native/JIT accepted and rejected fixtures through the
real load and stripped image paths, without adding TRUST or unchecked seams.
