---
title: "Split a stored quotation type's str in a split-str file"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:31:01.463565+01:00"
---

Problem: in a file marked split-str the checker reads str as ^u8 n in a signature, but not in a quotation type that TYPED-VARIABLE or a STRUCTURE's FIELD stores: with 'TYPED-VARIABLE V [ str -- ]', ': F ( ^u8 n -- ) V @ execute ;' is E-MISMATCH (execute takes str; the stack holds ^u8 n), and storing a quotation over ( str -- ) into V fails the same way, though docs/forth.md Types says a marked file reads each str in the rows it checks against as ^u8 n. Tender met it respelling its strings (2026-11-19): nine stored quotation types there stay ^u8 n until this is fixed. Reproducer: hb --load on a file holding split-str, require src/lib/prelude.f and those two lines (hb a45a6990). Acceptance: the reproducer and its FIELD twin load in a marked file, an unmarked file still refuses them, and test/split-str-test.f pins both. Depends: none. Claim: agent=kevin workspace=.jj-ws.noindex/kevin-split-quot
