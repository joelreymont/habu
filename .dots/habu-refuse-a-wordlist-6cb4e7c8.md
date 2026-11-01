---
title: "Refuse a wordlist id past WID:MAX where ids are made"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:36:12.521118+02:00"
---

Problem: wordlist ids are dense counters (WIDN-CELL), and nothing that makes one checks WID:MAX (layout.f WID:MAX = $FFFFFFFE): habu1.f BWORDLIST (~4312), habu2.f NAMESPACE-RECORD (~4044) and C-PACKAGE-ALLOC-WIDS (~9681). The only refusal of id exhaustion is the generated-declaration preflight (src/core/generated-declaration-protection.f ~100), against its own WID-MAX of $FFFFFFFF, one past the layout's. Acceptance: the allocator that hands out an id refuses one past WID:MAX with a named engine error, at both tiers and in the Habu loop; the preflight's private bound goes. Found reading the code (/tmp/jerry-big/protwid-design-opus.md). Files: src/habu/habu1.f, habu2.f, outer.f, src/core/generated-declaration-protection.f. Verify: a row that sets WIDN near the bound and makes two wordlists; candidate build. Related: habu-keep-prot-wid-3dd32ed9. Claim: unassigned.
