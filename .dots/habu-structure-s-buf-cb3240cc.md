---
title: "STRUCTURE's BUFFER takes a count word"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T11:05:31.486900+01:00"
---

Problem: stuart (Tender) 2026-11-22, reproducer /tmp/stuart-buffer-const.f: `BUFFER id ID-BYTES` in a STRUCTURE is E-BAD-DECLARATION (src/core/structure-decl.f BUFFER-COUNT takes a number literal only), while TYPED-BUFFER and LAYOUT-BUFFER take a literal or a ( -- n ) word (docs/forth.md "A fixed buffer's count"). Tender keeps a load-time check beside each literal. Acceptance: BUFFER takes its count by the same rule as TYPED-BUFFER; the reproducer loads; a warm build after the count word changes equals a cold one; forth.md and the card state the one rule. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-bufcount.
