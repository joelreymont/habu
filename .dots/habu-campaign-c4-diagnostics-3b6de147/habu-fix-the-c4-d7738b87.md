---
title: "Fix the C4 third re-review's findings"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T19:15:03.419892+01:00"
closed-at: "2026-11-16T04:07:36.133099+01:00"
---

Problem: the fix re-review of szyqoqqo..mnkunwml found: test/dictionary-full-test.f reaches no namespace or EXPORT full-dictionary site and its header misplaces a does> clause; structure-make.f's provisional-rows comment says reject where a reader dies 76; engine-suite RSV46 and RSN7 pin part of their help line; tools/source-discovery.f SD-X-KEY aliases records from file 65536 on; the merge oqpzxmxv kept the old closers in the unclosed-row help, refuse in three comments, codes in the record-cap docs and the REGISTER-TRUST pass-through; verify-source.f says packet; no test reaches a caught code through >r r>, two counted catches in one definition, or a caught local thrown from a quotation; t0.f THE THROW ON's header points at another file's word. Acceptance: each fixed; dictionary-full, engine, hashmap, source-discovery and diag-position pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
