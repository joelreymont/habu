---
title: Collapse build records into one inputs-to-product shape
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:18:15.979408+03:00"
---

Problem: twelve cache layers and five hand-rolled key+rows record formats (HBMSEL1! tools/hb-build-lib.f:649-654/671-676, HBENGIN2 :1404-1425/1527-1533, HBRDEPS3 :1625-1648/1782-1804, HBRSEL2! :1680-1697/1711-1722, HBNSUP1! tools/native-build-image.f ACCEPT-RESULT + tools/native-build-result.f) and eight CONTENT-KEY schemes on the AOT path. The promise needs definition containers, one inputs->product record per path, the product artifact and the builder images. Acceptance: one record shape for every family; maker-support (:625-690) and repl-support (:1613-1739) records dropped if measuring the closure hash shows them not worth keeping; behaviour and the existing hit/miss E2Es unchanged; wall time of a warm build no worse. Files: tools/hb-build-lib.f, tools/native-build-*.f. Depends: Hit the artifact cache after reverting an input; Delete the duplicate object cache. Claim: unassigned.
