---
title: "Publish the payload's instantiated cells for glue"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.836548+02:00"
---

Pre-existing narrowing found by the ctor-pads landing (not made worse by it): both construction paths compute the bundle's base from the DECLARED payload cells, so a wider-than-declared payload leaves the glue run short at the bottom. Today: right by accident when the payload's bottom value is itself glued; where not (VARIANT v n a at <pt>), a spurious BUNDLE-CK refusal - never a wrong program (the checker refuses renames into bundles independently, measured). Fix needs a SECOND instantiated number (the payload cells) beside the pad difference - one published value cannot carry both. Files: src/core/type-family.f (latch), src/compiler/native/{dict,elaborate}.f. Depends: the ctor-pads landing (worked example).
