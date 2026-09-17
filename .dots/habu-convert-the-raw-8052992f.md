---
title: Convert the raw pointer cells in lib and tools
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.878382+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) lists the lib/ and tools/ sites the rule habu-refuse-a-ptr-5ad2734e refuses: lib/fs.f first (fs-check-record at FS-DIRENT-NAME-END is the first refusal a rule engine hits, and it blocks test/pointer-storage-test.f, test/typed-storage-test.f and test/internal-word-gate.f on that engine), tools/engine-size.f (17), and the rest of the mechanical shapes there (ptr-field on a raw base; pointer stored into a raw cell; raw cell fetched then dereferenced; here or data-base fetched into a raw cell; indexed raw table of pointers). Acceptance: every lib/ and tools/ site of a mechanical shape converted to the declared form the census names (PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, TYPED-BUFFER; a cell named directly where 0 ptr-field was the identity), with the 'other' and AMBIGUOUS rows in those directories converted where the body makes the form obvious and otherwise listed for habu-convert-the-hand-judged lane with the reason; each converted file's closure loads on ~/.cache/hazel/engines/raw-rule-gen1 without E-RAW-CELL-PTR, starting with lib/fs.f so the three blocked suites run there; the same suites green on the release engine; byte fixpoint for the baked lib files. No TRUSTED: or unchecked seam is a conversion. Files: lib/, tools/ per the census. Verify: the affected suites on bin/hb and raw-rule-gen1; fixpoint; test/run.f. Depends: none; lands before the rule. Ownership: lib and tools. Parent: habu-refuse-a-ptr-5ad2734e. Claim: agent=hazel-raw-libtools workspace=.jj-ws/hazel-raw-libtools.
