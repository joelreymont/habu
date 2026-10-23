---
title: Attribute unowned DATA honestly
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.419689+03:00"
---

The old RUN-ROW width demand is obsolete: capture now has distinct named
XTSITE, PWIN and packed BOOTRUN formats. Do not restore an old shared width.

The remaining reporting issue is tools/data-table-census.f's attribution of
anonymous storage after the last declared object to that preceding object.
The current size tool is a
wrapper over image-size-lib, so the old TR-LASTZERO/USIGS-USER comparison is
not a current measurement.

After build/test work, compare the tools on one matching engine/map before
changing them. Where declaration/allocation metadata establishes ownership,
report that owner; where it does not, report anonymous storage rather than
guessing from its predecessor. Do not add an ownership registry or infer an
owner merely from pointer-like values. Preserve totals and verify the actual
mislabelled extent with the smallest existing sizing artifact. This task
claims reporting accuracy, not a code-size or runtime improvement.
