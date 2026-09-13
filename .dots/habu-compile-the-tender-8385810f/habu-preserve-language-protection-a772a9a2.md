---
title: Reject internal JIT calls even when a typed effect is available
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T16:24:10.390520+03:00\""
---


Confirmed cause: native source replay retains an active declaration owner before
the replacement checker hooks are enabled. `LASTC-TRUST:PUBLISH-PTR-A` records
`-- ptr a` for create/variable, and `TRANSFER-CHECKED` preserves those effects.
Original host `28e11361` had no effects for `NULL-PTR-CELL` or `PF-COMMIT-N`;
unchanged-source `ccc0661a` product `45b5b4eb` has pointer effects for both.
Their `DNAME-INT` flags survive unchanged. JIT `EM-COMPILE-CALL` failed to check
that flag, so a checked NULL-cell store certifies and executes once its effect is
known. Tier 1 already rejects the same call through `NDICT:CALL-TARGET`.

Repair the JIT's shared found-target path before an immediate executes or a call
is emitted, allowing internal calls only under explicit `TRUSTED:` compilation.
Preserve the useful effects and existing bare/tick/search protections. The
existing native-internal-call fixture now gives a harmless child-owned internal
word a known checked effect and tests rejection plus trusted execution on both
tiers. The earlier engine rejection changes seed-ndict!'s whole-load diagnostic
from the checker's trusted-capability error to E-UNDEFINED; its direct checker
verdict remains tested independently.

The owner-suite failure was assertion188, candidate `TFO-N3 ( -- n ) PF-COMMIT-N`.
The new pointer effect correctly rejects that number result; this was not an
owner rollback or retired-token failure. Declare its real `ptr n` result so the
neighbor assertion continues to test exact retired-name matching. The retired
name refusals remain unchanged.

Files: `src/habu/habu2.f` JIT call guard and the existing internal-word,
native-internal-call and type-field-owner suites. Recovery emits this same native
source; transient stage0 records carry no DNAME-INT field to mirror. No checker,
source-owner or tier-dispatch changes are needed for this repair.

Acceptance: first and repeated product-hosted builds preserve these rejection and
owner behaviors, followed by the required native suite. Private source `799f713b`
plus this repair builds from `968cabff` to `2b931ed6` and then `541586f5`, both rc0.
All three focused suites pass on both products. The integrated full suite remains
pending. These builds are functional evidence, not a byte-fixpoint claim.
Binaries and diagnostics are preserved under
`/tmp/cedar-native-baseline.hJ8uHu`. Cedar independently reviewed the source and reran all three focused suites on the repeated product: all passed. Integrated full-suite verification remains pending.
