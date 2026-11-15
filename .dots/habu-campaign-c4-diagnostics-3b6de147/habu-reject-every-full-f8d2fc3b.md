---
title: Reject every full dictionary as E-DICTIONARY-FULL
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.327093+01:00"
closed-at: "2026-11-16T04:07:34.449155+01:00"
---

Problem: a declaration that fills the dictionary prints E-BAD-DECLARATION with exit 70 (src/core/generated-declaration-dictionary.f:10-14, :46-48), and package, EXPORT or a qualified head names and places the last definition (src/habu/interpreter.f:860-867); no test covers a full dictionary. Acceptance: each prints one E-DICTIONARY-FULL record naming the word it makes, exit 77; a test covers definition, does>, package and declaration. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
