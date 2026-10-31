---
title: Pass a fresh-region view to a region-generic word
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T14:38:57.809219+02:00"
---

Problem: inside a definition, a view whose region is a declared fresh-region atom cannot be passed to a word whose effect has a region variable in that slot. Reproducer (lane c2cb, measured in c2-owner.f on the master engine): ': Q5 ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) ;  : P5 ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-b,u8> ) Q5 ;' is refused 'expected mut-view<a,b,c,u8> actual mut-view<d,e,fresh-region-b-a,u8>', 'scope parameter: a scope identity cannot stand in for a value type' (MD-SCOPE-KIND, SCOPE-HIT 2 in src/core/checker.f SCOPE-BLOCK?); the same body with region a or <p,p,a> passes. It blocks habu-check-the-c2-99fd69f8 (ALLOC-DISPOSE). Acceptance: the root cause found and named (which variable or atom reaches SCOPE-BLOCK? with which kind, and why); if admitting P5 is sound for every program, the fix at that layer with an accepted row for P5 and a rejected row for each kind edge the fix borders, each failing on the master engine; if it is not sound, the reason measured and stated beside the rule, and no change. Files: src/core/checker.f, a checker test row. Verify: engine build; the new rows; the rows that own region and scope kinds. Depends: none. Ownership: checker.f region/scope kinds. Brief /tmp/jerry-big/tcl-frgn-brief.md. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-frgn
