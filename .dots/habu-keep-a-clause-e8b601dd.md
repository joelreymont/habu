---
title: "Keep a clause's facts on the made word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:34:25.776025+01:00"
---

Problem: a word a definer makes drops the facts of its does> clause: with ': DF ( -- ) create does> ( -- ) drop parse-name drop ;' and 'DF W1' the word ': USE ( -- ) [: W1 ;] KV ! ;' loads, so KV, a TYPED-VARIABLE of [ -- ], holds a quotation that parses. Acceptance: load and check reject USE as they reject the same body with the clause's text in place of W1; a diag-position case fails without it. Depends: none. Claim: unassigned. Root cause and a proposal: .out/designs/made-word-facts.md (root checkout).
