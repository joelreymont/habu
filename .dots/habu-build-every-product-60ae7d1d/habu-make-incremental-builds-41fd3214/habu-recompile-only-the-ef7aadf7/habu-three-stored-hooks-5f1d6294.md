---
title: Three stored hooks carry a body edit into parts that never call the edited word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.631595+02:00"
---

Problem: checker.f:13362 defer FIRST-RECORD-XT is filled by src/core/prefix-boundary.f:22 only because checker.f loads first; type-family.f:7418 stores ' TFAM:TFL-VAR? into a DECLARATIONS cell; decl-event.f DEV-EVENT-HOOKS-INSTALL stores a quotation. AGENTS.md Machinery being replaced lists a hook where the owner could make a plain call or record data. Each makes every part that reaches the holder's data depend on the hook's callees (66, 22 and 4 parts for a CORE-FOLD-C body edit). Acceptance: each of the three is a plain call or recorded data, the defer or cell is gone, and the util.f body edit's rewritten count is reported before and after each. Files: src/core/checker.f, src/core/prefix-boundary.f, src/core/internal-mark.f, src/habu/type-family.f, decl-event.f. Verify: engine builds twice, test/file-objects.f rc 0, the census count. Depends: none (the store-reach dot removes the cost first; this removes the hooks). Ownership: those words. Claim: unassigned.
