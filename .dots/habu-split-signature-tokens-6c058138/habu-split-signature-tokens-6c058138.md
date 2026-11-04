---
title: Split signature tokens at a newline
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:25:14.200063+02:00"
---

Problem: a signature that runs over two lines keeps the newline inside a token. /tmp/jerry-dtailrr2-tmp/p/cml.f (`: CML ( n<newline>    zz-unknown ptr a -- n ) drop drop 5 ;`) on the public hb prints `habu: in cml: unknown type 'n<newline>' in signature` and `hook: non-certified definition: cml at 'n<newline>'`, rc 70; mkl.f (the same inside a `does>` clause's signature) prints the same token on lane dtail's candidate. The reader collects the signature text up to `)` across the line, then splits it at spaces only, so `n` and the newline become one token. Acceptance: inside a signature a newline separates tokens as a space does, for a colon definition and a does> clause, in the engine loop, the Habu loop, tier 0, tier 1 and tools/check.f alike; cml.f and mkl.f then name `zz-unknown` as the unknown type, and the same source with a known type on the second line loads. Files: the signature token splitter(s) the five paths share (find each reader before editing: src/core/checker.f signature parse, src/habu/habu2.f C-PARSE-CREATED-SIG, src/compiler/t0 signature reader), test/certify-does-definer.f or test/outer-interpret.f for the rows. Verify: the two probes on the candidate and the installed hb; the rows of the files touched. Depends: none. Related: habu-agree-where-a-aceab3bb (where a signature starts). Claim: unassigned.
