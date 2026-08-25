---
title: Stop the VERIFY tokenizer miscounting quoted comments
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T05:46:50.935857+02:00"
---

Found by the create-axiom lane: tools/build-fixpoint.f VERIFY's tokenizer miscounts on a double-quote inside a backslash comment - inserting one comment line moved CENSUS-COUNT by six phantom colon definitions; checker.f already carries 49 such lines. The same tokenizer backs BF-CERTIFY-GENERATED, the gate whose stated job is that a type error in emitted engine source cannot warn its way into an installed binary - a lexing hole in that gate is a coverage hole, not cosmetics. Fix the tokenizer's comment handling with a fooling fixture (quote in backslash comment, in paren comment, escaped); re-pin the published counts with derivation. Files: tools/build-fixpoint.f. Depends: none.
