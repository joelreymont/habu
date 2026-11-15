---
title: Close a primitive row where the engine closes it
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:14:23.761152+01:00"
---

Problem: two tools read src/habu/prims.f rows besides the engine, and they disagree on parsing words in a row body. src/lib/source-lex.f PARSE-NEXT? lets only [char] and ['] consume the next token; src/habu/verify-source.f SKIP-BODY-TOKEN (via BODY-PARSER?) also consumes the token after postpone. So 'EPRIM: FOO postpone EPRIM; PE-N EPRIM;' closes at different closers (found by reading, not run). The engine interprets a row body, so every parsing word there (char as well) consumes its operand. Acceptance: both tools close every row at the token where the engine closes it, or a row body that holds a parsing word is rejected in all three; a source-lex case and a verify-source case with postpone before a closer. Depends: none. Claim: unassigned.
