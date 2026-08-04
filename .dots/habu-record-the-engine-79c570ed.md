---
title: "Record the engine's own literal values on the tape"
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T18:28:09.620769+02:00"
---

Full context: src/compiler/native/feed.f INT-VALUE reads an integer literal's value back with lib/string.f STR>NUMBER? and refuses (E-NFEED-LITERAL) every spelling that reader declines, so a hexadecimal literal ($FF), an out-of-range literal, and a float literal (E-NFEED-KIND, no tape kind) cannot be recorded today. The authority on what value a literal token pushes is the engine's own number parser (src/habu/habu1.f EMIT-NUM/C-NUM-DOT); the checker mirrors its LANGUAGE in ALLDIG?/FLODIG? but not its VALUE, and no prim exposes the conversion to Habu, so the producer has a second decimal decoder where there should be one authority. Acceptance: the tape's literal cell carries the value the engine itself parsed for every literal spelling the engine accepts - decimal, hexadecimal, character and float - with no decoder of its own in feed.f; test/compiler/native-feed.f pins each against the value the compiled word actually computes; E-NFEED-LITERAL survives only for spellings the engine itself rejects. Needs either a checked prim over the engine's parser or a literal-value field on the checker's token event. Depends on habu-feed-the-src-f7ed8733.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-feed-the-src-f7ed8733 is
no longer in the graph: it was closed and archived by commit 36fe20b26 "Close the
tape-producer dot, dot the cert seal", so the source-tape producer this dot records onto is
landed and the dependency is satisfied. Nothing blocks this dot now.
