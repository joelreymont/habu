---
title: "Answer a word's effect as a declaration writes it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:39:51.635191+02:00"
---

Joel approved 2026-11-05 (relayed by tim, Maki commit "Record the vocabulary effect decision"; design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/vocab/f2-design.md): public word EFFECT-TEXT$ ( ptr u8 n -- ptr u8 n bool ). For a word name, the effect the checker holds (EFFECT-QUERY, src/core/checker.f:14241) as a declaration in any other package writes it: din -- dout, plus | rin -- rout; each named package's family as PKG:tail in the package's own spelling (TFAM-PKG$), global and @ families bare; false and empty text when the checker has none. Today render.f FAM-QNAME-REND (:254-264) qualifies relative to TFAM-ACTIVE-PKG$ and folds the package (RFOLD); JEFFECT-TEXT (:574-578) renders a row. Caller: Maki src/vocabulary.f DOC-ENTRY. Acceptance: the text of a word whose effect names another package's family checks when pasted into a declaration in a third package; a prim row's effect renders; an unknown name answers false. After Maki uses it, WORD-DOCS:EFFECT$ (src/lib/word-docs.f:226-237) has only its test as a reader. Ownership: carl. Claim: unassigned.
