---
title: Prepare origin keys before publication
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T07:19:17.005287+03:00"
---

Independent adversarial review of the FILE origin codec at83530bcd found nominal origin keys and required owner callbacks are first resolved during INSTALL, after DATA/code/dictionary publication. A framed row with a nonexistent nonempty symbol key can pass PREPARE then throw after mutation; FULL header can similarly lack FILE-DRAIN/RESTORE. Validate through the checker prepared plan and projected dictionary before publication, without live interning, duplicate checker parsing, or rejecting valid new FILE symbols. Move existing origin PREPARE after CHECKER-PREPARE in automatic and explicit import; preserve current range reference table. Add a narrow checker prepared-symbol query over existing decoded UP plans and an object projected-callable query honoring newest namespace/retirements and extended names. Keep actual live symbol lookup at INSTALL. Existing malformed-section E2E must prove unchanged publication state for invalid nominal/provider cases and acceptance of new planned symbols. Carl owns integration with independent Astra review, matching native FILE and shared-core gates.
