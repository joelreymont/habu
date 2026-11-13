---
title: "Ask for an open row's children when it is sent again with more"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T12:14:39.929976+02:00"
---

Problem: src/lib/browser/page.js:107 restores a row's open state by key, so a row that was open and is sent again with more: true and no children shows open and empty; it asks for its children (page.js:71) only after a close and a reopen. No module sends such a row today; Maki's lane E (viewer.f BUILD-TREE, design /home/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/design-view.md section 4) will be the first. Acceptance: such a row asks for its children once, as an opening does, or shows closed; the headless Chrome peer re-sends an open row with more and sees the request. Depends: Maki lane E sending more. Claim: unassigned.
