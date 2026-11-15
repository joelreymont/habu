---
title: Name the generating family in a duplicate definition
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T10:10:20.343749+01:00"
---

Problem: a word whose name a type family generated (a private STRUCTURE's FAMILY-MAKE today; every private ENUM or SUMTYPE variant once habu-derive-on-a-d47ec839 lands) is rejected as 'duplicate definition: NAME' with no earlier definition in the source, so the agent cannot see what to rename. Acceptance: when TFAM-CTOR-WORD? answers for the name, the rejection's help names the generating family and variant and says to rename the word. Depends: none. Claim: unassigned.
