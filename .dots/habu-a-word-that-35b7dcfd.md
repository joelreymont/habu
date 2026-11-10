---
title: A word that loads a file during a build makes a stale warm build
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:48:53.154508+02:00"
---

Problem: a build compiles its own copy of src/core/include.f into the space it writes; a word of the build's files that calls included or required, run at a file's top level, calls that copy, which loads the file without the linker seeing it (src/core/include.f included, required; the build's SOURCE-UNIT runner is hb's only). The loaded file's definitions land in the requesting file's object and no object names the file, so after the loaded file is edited a warm build links the stale object. Shown on master's installed hb (carl's 793edbff): inc.f defines HELLO; inc3.f is 'require inc.f' and ': HELLO3 ( -- ) HELLO ." three" cr ;'; c-main.f is ': L ( -- ) s" inc3.f" included ;  L  : MAIN ( -- ) HELLO3 ;'; build, edit inc3.f to print four, build again into the same out root: the program prints three; a cold build prints four. On the process-state stack the same source dies of SIGSEGV (dot f2046e55, the same cause). docs/forth.md already says a word in a file a build loads never loads a file. Acceptance: the build refuses the load at its entry with a message that says to load the file at the file's top level; test/file-objects.f holds the case. Files: src/core/include.f, test/file-objects.f, test/fixtures/file-objects/word-load-main.f, word-load-inc.f, docs/forth.md. Verify: test/file-objects.f WORD-LOAD-BUILD; the three-file reproducer exits nonzero with the message. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-crash (brief .jj-ws.noindex/jerry-wasm/.out/briefs/load-refuse.md).
