---
title: A stored hook carries a body edit into parts that never call the edited word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.631595+02:00"
---

Problem: src/core/checker.f:12376 defer FIRST-RECORD-XT is filled by src/core/prefix-boundary.f:22 only because checker.f loads first. docs/design.md "Machinery being replaced" lists a hook where the owner could make a plain call or record data. It makes every part that reaches the holder's data depend on the hook's callees (66 parts for a CORE-FOLD-C body edit). Acceptance: it is a plain call or recorded data, the defer is gone, and the util.f body edit's rewritten count is reported before and after. Depends: none (habu-a-store-brings-1ba604bf removes the cost first; this removes the hook). Claim: unassigned.
