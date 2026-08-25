---
title: Logical-shape depth and .s over layout rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T13:28:23.595561+02:00"
---

TFAM 12 item (5) verdict 2026-07-09: depth/.s stay PERMANENTLY fail-closed over any row holding hidden physical fields (checker.f HIDROW-STEP?, regressions TD12-DEPTH/TD12-DOTS in test/type-decl-suite.f assert reject) — docs/type-families.md section 17 sanctions reject as the alternative to logical-shape reporting. This dot is the lift: teach depth/.s to report the LOGICAL stack shape (count/render whole bundles as one value via the row width facts, never exposing @family.slotN/@family.tag), flip TD12-DEPTH/TD12-DOTS to certified, and prove runtime behavior with execution rows in test/type-layout-lower-pending.f.
