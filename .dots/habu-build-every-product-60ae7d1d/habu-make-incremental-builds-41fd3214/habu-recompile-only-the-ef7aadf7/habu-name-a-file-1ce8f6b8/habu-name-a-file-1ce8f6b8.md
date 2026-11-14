---
title: "Name a file's data by the word that owns it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:57:17.141281+02:00"
---

Problem: a data reference names an extent of a part's data (src/habu/linker.f), and the design's lanes are its children; not yet shown is that the string and variable edits of the six-edit table (in util.f and repl.f each: a comment; a new unused word at the end; a new unused word mid-file; a body edit of one word; a string literal made longer mid-file; a new variable mid-file) compile only the edited file and the files that use a changed word. The util.f string edit rewrites 11 objects; which of them use no changed word is not read. Acceptance: the string and variable edits of the six-edit table, in src/core/util.f and src/habu/repl.f, compile the edited file and only files that use a word whose code, facts or data changed; warm equals cold unsigned; every reference of the census has an answer. Depends: none. Claim: unassigned.
