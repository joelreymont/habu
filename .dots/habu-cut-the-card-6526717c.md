---
title: Cut the card to what a writer cannot derive
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:14:29.194132+02:00"
---

Problem: docs/forth-card.md is 609 lines, 42 KB, and AGENTS.md tells every agent to read it before writing Habu; docs/forth.md is 2700 lines, 160 KB, read by heading. Joel, 2026-11-10: "should forth-card.md and forth.md be rewritten to be concise without losing precision?" Measured (/Users/joel/Work/reviews/habu-10x-2026-11-10/measure-r.md): language documents are 3.6% of tool-result characters, the median transcript that reads them takes 4.3k characters, and 351 of 536 episodes that wrote an absent word had never opened the card. So the card is the one worth cutting, and only by removing what is duplicated, derivable or said twice; forth.md is paid for by heading and is corrected where a section is found wrong, not rewritten. Acceptance: every rule, refusal row and example in today's card is either in the new card, or listed in the commit's evidence as removed with the reason (duplicated at card line N, or stated at the forth.md heading the card names); no rule changes meaning; every example that is code loads, or is refused with the code the card gives, on bin/hb; section 10's headings all exist in forth.md. No size target. Files: docs/forth-card.md. Verify: the rule-by-rule list; the examples run; a code-reviewer checks the list against both versions. Depends: the lanes that add card rows landed (3152182a, e0ec22ab, 0c34ca08), so the cut starts from a quiet file. Ownership: the card. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-cut-the-card-6526717c
