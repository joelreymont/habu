---
title: "Record the engine's own literal values on the tape"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-31T18:28:09.620769+02:00\""
---

Full context: src/compiler/native/feed.f INT-VALUE reads an integer literal's value back with lib/string.f STR>NUMBER? and refuses (E-NFEED-LITERAL) every spelling that reader declines, so a hexadecimal literal ($FF), an out-of-range literal, and a float literal (E-NFEED-KIND, no tape kind) cannot be recorded today. The authority on what value a literal token pushes is the engine's own number parser (src/habu/habu1.f EMIT-NUM/C-NUM-DOT); the checker mirrors its LANGUAGE in ALLDIG?/FLODIG? but not its VALUE, and no prim exposes the conversion to Habu, so the producer has a second decimal decoder where there should be one authority. Acceptance: the tape's literal cell carries the value the engine itself parsed for every literal spelling the engine accepts - decimal, hexadecimal, character and float - with no decoder of its own in feed.f; test/compiler/native-feed.f pins each against the value the compiled word actually computes; E-NFEED-LITERAL survives only for spellings the engine itself rejects. Needs either a checked prim over the engine's parser or a literal-value field on the checker's token event. Depends on habu-feed-the-src-f7ed8733.

Claim: agent=litauth workspace=.jj-ws/habu-literal-auth

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-feed-the-src-f7ed8733 is
no longer in the graph: it was closed and archived by commit 36fe20b26 "Close the
tape-producer dot, dot the cert seal", so the source-tape producer this dot records onto is
landed and the dependency is satisfied. Nothing blocks this dot now.

CORRECTION 2026-08-12 (litauth, measured in the tree). The leaf's float sentence is stale:
a float literal HAS a tape kind and has been recorded since real-lit.f landed
(src/compiler/native/tape.f real-literal, feed.f APPEND-REAL, and the REAL/NEG-ZERO/AWKWARD
cases in test/compiler/native-feed.f). E-NFEED-KIND now fires only for a token class the
reader does not have. So floats were never a missing kind - they were the second reader:
src/compiler/native/real-lit.f re-derived the double by reproducing the engine's own float
route instruction for instruction, which is the same fault as INT-VALUE's decimal decoder
and is repaired the same way. That file is deleted by this dot's commit.

SHAPE, decided by measurement rather than by preference. The value does not exist when the
tape event is recorded: the engine reads a literal while it COMPILES the body
(src/habu/habu2.f EM-COMPILE-LITERAL, at label LNUM) and spends the answer immediately on a
push instruction, while the tape's event fires later, from the checker's own scan of the
reconstructed definition text at the `;` hook. No engine surface keeps a parsed value in
between - even the top-row literal event (src/habu/layout.f TOP-EV-NUM) hands its hook the
token's bytes and never the number. A value field on the token event would therefore either
re-run the parser anyway or correlate two token streams by position, so the shape is the
other one the leaf names: the engine's routine is exposed as the `num-parse` primitive
(src/habu/habu1.f ENGINE-EMIT:BNUMPARSE, axiom row in src/core/checker.f) and feed.f asks
it. E-NFEED-LITERAL keeps one job: the reader classified this token with the checker's
predicates and the engine's own reader disagrees, which is the two literal languages
drifting apart.

The two capabilities this dot deliberately does NOT deliver are dotted: a character
literal's code point on the tape is habu-a-char-literal-4c8ecc3b (the reader skips a
character literal's payload, so it needs a reader class and a tape kind, not a value
lookup), and moving the checker's literal CLASSIFICATION onto the same authority is
habu-one-authority-for-252cf73b (that changes the literal language for every checked file).
