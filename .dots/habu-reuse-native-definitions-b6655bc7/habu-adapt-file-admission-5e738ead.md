---
title: Adapt FILE admission probes to checked top
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T16:41:47.230544+03:00"
closed-at: "2026-10-19T17:00:42.346124+03:00"
close-reason: Preserved all assertions and top-level safety, using small trusted fixture boundaries for intentional hook/raw-code operations. Reader rc0 prints11; seven raw captures retain expected70 refusals. Independent Astra review clear. The separate deferred import ORDERED failure remains tracked as294827f3; this is not a full-suite pass.
---

Unchanged native-file-admission-e2e on matching4384 fails18 assertions. First generated reader.f exits70 before opener checks because it calls ETRUSTED-ONLY native-body-reuse-cell at top level. Preserve the new top-level safety rule and every behavioral assertion; put intentional hook install/removal into small explicit trusted fixture boundaries defined before arming, then reduce any remaining failures separately. No production rule weakening or callback-count change. Existing E2E is pre-code failure; require real reader cursor/body/cache and image refusal checks plus independent review.

Fixture boundaries implemented: reader replay exits 0 and prints 11; the five
raw-storage probes reach their intended capture rejection, including cold and
warm builds, nonempty does>, new unaligned data and an unaligned prefix update.
All child rejections remain 70 and name an undeclared code/dict pointer. No
behavioral assertion or production rule changed. Keep active for independent
review and the lead's final native gate.

Separate residual: unchanged STRICT-DEFER exports successfully and its image
prints 456, but import exits 74 with dependency miss: ordered source action
(-8597), failing assertions 62 and 63. Exact reducer and verification logs are
under /Users/joel/.cache/tmp/carl-admission-fix.YKIhd4; no production fix was
attempted in this fixture task.
