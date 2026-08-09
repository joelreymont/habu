---
title: Put string and character payloads on the source tape
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:28:35.580289+02:00"
---

Full context: the reader src/compiler/native/feed.f hangs on (src/core/checker.f CHECK-SCAN) consumes a string or character opener as ONE name token and then steps over its payload bytes (SKIP-STRING-PAYLOAD, SKIP-PARSE-LIT-PAYLOAD), so a produced tape records that 's"' or '[char]' was read and records nothing about the bytes behind it. src/compiler/native/tape.f has kinds string-literal and char-literal - whose spelling is the literal BODY - and no producer can reach them; a later stage that needs a string literal's bytes must not guess them from the opener's span. Acceptance: a definition containing a string literal and a character literal records a string-literal row whose spelling is the body and a char-literal row whose literal is the code point, from a reader that actually consumes those bytes rather than skipping them; test/compiler/native-feed.f pins the recorded body against the string the compiled word pushes. Depends on habu-feed-the-src-f7ed8733.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-feed-the-src-f7ed8733 is no
longer in the graph: it was closed and archived by commit 36fe20b26 "Close the tape-producer
dot, dot the cert seal", so the source-tape producer this dot puts payloads onto is landed
and the dependency is satisfied. Nothing blocks this dot now.

STRING HALF LANDED 2026-08-09 with dot habu-compile-str-literals-30a7121b
(merged at 3ba76ff0): the reader consumes string payloads, decodes escapes in
the checker, and fills the tape's string-literal kind; test/compiler/
native-feed.f pins the recorded body against what the compiled word pushes,
including a re-lex-hostile fixture. REMAINING SCOPE OF THIS DOT IS THE
CHARACTER HALF ONLY: [char] payloads still do not reach the tape's
char-literal kind.
