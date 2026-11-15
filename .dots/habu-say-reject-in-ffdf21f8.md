---
title: Say reject in every name
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.195649+01:00"
---

Problem: names across src, tools and test still say refuse (REFUSED-AT, VERDICT-REFUSED, SUBJECT:REFUSED?, TIER0-REFUSES, BUILD-REFUSE, REC-REFUSE-WHY and more); Joel: 'replace refusals with rejections'. Acceptance: no name, label or comment says refuse where Habu rejects, and no comment calls what REJECTION prints a packet (rg -i 'refus|packet' over src, tools and test: checker.f 183 lines, verify-source.f 62, fs.f, repl.f:175 and :183, dict.f:48 among them). Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
