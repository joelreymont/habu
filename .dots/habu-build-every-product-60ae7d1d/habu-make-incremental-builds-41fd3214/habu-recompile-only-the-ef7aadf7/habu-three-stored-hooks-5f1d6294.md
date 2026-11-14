---
title: Three stored hooks carry a body edit into parts that never call the edited word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.631595+02:00"
---

Problem: src/core/checker.f:12416 defer FIRST-RECORD-XT is filled by src/core/prefix-boundary.f:22 only because checker.f loads first; src/core/type-family.f:7396 stores ' TFAM:TFL-VAR? into a DECLARATIONS cell; src/core/decl-event.f:640 DEV-EVENT-HOOKS-INSTALL stores quotations. docs/design.md "Machinery being replaced" lists a hook where the owner could make a plain call or record data. Each makes every part that reaches the holder's data depend on the hook's callees (66, 22 and 4 parts for a CORE-FOLD-C body edit). Acceptance: each of the three is a plain call or recorded data, the defer or cell is gone, and the util.f body edit's rewritten count is reported before and after each. Depends: none (habu-a-store-brings-1ba604bf removes the cost first; this removes the hooks). Claim: unassigned.
