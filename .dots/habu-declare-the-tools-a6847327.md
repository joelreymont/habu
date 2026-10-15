---
title: Declare tool storage needed by pointer rules
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.100828+03:00"
---

The tools retain private mixed pointer/count storage, including LINT-SLAB in
tools/lint/text.f and its users, and paired pointer/length helpers in the check,
build and JSON tools. Existing bounds checks do not establish the pointer-cell
type invariant needed by the open ptr-field and pointer-view rules (331a9731
and 030be3ad).

When those rules have a current design, migrate only the storage and accessors
needed to close their demonstrated laundering paths without breaking legitimate
tool callers. Use existing declared records where they express the layout;
preserve bounds, published sizes and behavior. Do not require every private
pointer/length pair to become a record or a tree-wide zero-ptr-field count.
Select the affected lint/check/build E2Es from the actual changed paths; shared
checker impact requires the repository native gate and matching engine.

The unused indexed CHK-PTR-U8-SLOT helper family is a separate small deletion
under 11e9a4f3. The used non-indexed pointer-slot helpers remain. This narrowing
does not claim the stricter pointer rules or tool migrations are implemented.
