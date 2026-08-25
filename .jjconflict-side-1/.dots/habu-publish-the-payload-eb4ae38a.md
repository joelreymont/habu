---
title: "Publish the payload's instantiated cells for glue"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.836548+02:00"
---

Pre-existing narrowing found by the ctor-pads landing (not made worse by it): both construction paths compute the bundle's base from the DECLARED payload cells, so a wider-than-declared payload leaves the glue run short at the bottom. Today: right by accident when the payload's bottom value is itself glued; where not (VARIANT v n a at <pt>), a spurious BUNDLE-CK refusal - never a wrong program (the checker refuses renames into bundles independently, measured). Fix needs a SECOND instantiated number (the payload cells) beside the pad difference - one published value cannot carry both. Files: src/core/type-family.f (latch), src/compiler/native/{dict,elaborate}.f. Depends: the ctor-pads landing (worked example).

CORRECTION (bundle-seams landing 15baa001): the boundary-bit
encoding does NOT carry this leaf's second number for free - the
narrowing lives in the SITE's width (declared payload cells plus
the checker's pad difference), not in the encoding; the run is
still short at the bottom wherever the payload's own bottom value
is not glued (neither fixed nor worsened - master's right-by-
accident behavior preserved exactly). What the encoding DID close
free: two adjacent bundles are distinguishable, so MATCH over the
upper of two compiles. Also now pointing here: the zero-population
E-NELAB-MATCH refusal for an arm payload of 2+ fields in more
cells (pinned with engine twins in native-rename-rows.f) - this
leaf's number retires that boundary too.
