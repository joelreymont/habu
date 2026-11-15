---
title: "Fix the C4 fourth re-review's findings"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:09:33.339103+01:00"
closed-at: "2026-11-16T04:07:36.157539+01:00"
---

Problem: the fix re-review of musyvswn..kmwpswzw found: the record of an unclosed primitive-axiom row has two writers (src/habu/verify-source.f ROW-UNCLOSED, tools/check-all-errors-core.f CA-LEX-ROW-RECORD) and no test prints it; src/lib/hashmap-test.f HMT-PAIRS shows only one cell of a pair cannot answer it; test/dictionary-full-test.f's header omits the namespace and EXPORT cases; verify-source.f RECORD-NEWTYPE's comment says packet; test/diag-position-test.f's rethrow comments restate the cases and miss why TEST-RETHROW-TWO keeps two slots. Acceptance: one public VERIFY:ROW-REJECT writes the record for both, a diag-position case pins it under the check and --all-errors; the rest fixed; hashmap and diag-position pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing); the row record in the fifth merge (habu-merge-master-s-58d6bb7b).
