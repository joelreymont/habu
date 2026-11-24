---
title: "Cut the JSON value walk's rescans"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.537404+01:00"
---

Problem: a full EACH/EACH-MEMBER walk over a parsed value costs about five times a token pass (spark, /tmp/tim-jvbench/big.json 5.7 MB: token walk 60 ms, PARSE 128 ms, PARSE plus full walk 442 ms; scripts /tmp/jrd-bench/), because each level scans its elements again. Acceptance: a full walk of a parsed value costs no more than about one token pass plus PARSE, measured on the same file. Depends: none. Claim: unassigned.
