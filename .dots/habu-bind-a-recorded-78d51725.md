---
title: Bind a recorded unit source to its file origin
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:28:35.593955+02:00"
---

Full context: src/compiler/native/feed.f registers the text the checker's reader consumed - the engine's reconstructed definition - as the unit's IR source, so every span on the tape is an offset into that text and the content digest covers exactly those bytes. That text is not the file: backslash comments are gone, whitespace runs have collapsed, and the leading ':' and trailing ';' were consumed before the hook. A diagnostic that wants a file and a line therefore cannot get one from the tape, and design section 7.1 wants diagnostics reading the same tape as everything else. IR-SOURCE:REGISTER-FROM already records an include/expansion parent, which is the shape this needs: the unit's source registered as a child of the file's source, plus the offset of the definition inside it. Acceptance: a unit recorded from a file load has a source whose parent is that file's registered source; a span on the tape maps to a byte offset in the file; a test compiles a definition from a file and reads the file position of one of its tokens back. Depends on habu-feed-the-src-f7ed8733.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-feed-the-src-f7ed8733 is no
longer in the graph: it was closed and archived by commit 36fe20b26 "Close the tape-producer
dot, dot the cert seal", so the source tape this dot binds a file origin onto is landed and
the dependency is satisfied. Nothing blocks this dot now.
