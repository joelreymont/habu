---
title: Preserve prefix identity for named code literals
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T04:04:40.958930+03:00"
---

Source-confirmed during independent 4ccf56d9 design review: ACAP-OUT-CHAIN (src/habu/aot-capture.f) accepts any exact entry from ACAP-TGT>REC and stores the selected record bare name in AOT-XTSITE. The reverse index deliberately selects the lowest alias, but this path omits call-site ACAP-SITE-BAND, scope qualification and the exact name-to-xt resolution proof. A private/public alias, same spelling in another package, or excluded tooling target can therefore be misrepresented or fail only when seeding. Reduce actual prefix-literal controls, then reuse the responsible name/band admission contract while preserving internal literal relocation. Require executed global and public-prefix aliases plus same-spelling/private/prelude/non-entry rejection and producer/read/seed preservation. Distinct from eight-byte address-cell named targets (4ccf56d9); do not treat its fix as acceptance of this existing literal path.
