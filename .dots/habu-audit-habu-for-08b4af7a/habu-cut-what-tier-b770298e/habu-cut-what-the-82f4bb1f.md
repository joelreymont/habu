---
title: Cut what the tier-1 review found
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.267908+01:00"
---

Problem: the review of the merged tier-1 line found CHECKER-OWNER:SCAN-INCOMPLETE?, a one-word alias of checker.f's SCAN-INCOMPLETE; checker.f WF-WIDE? through WF-NEEDS-P2? and TYPE-FIELD:SLOT@, which only the deleted layout-valid.f reached; a native-hookless-reject case that passes in either order; CODE-DUMP:JD's JIT initials; and three tests still written for tier 1. Acceptance: each is deleted or made true, rg -i --no-ignore over src, tools, test and the three consumers finds no other caller, and the edited tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t1fix.
