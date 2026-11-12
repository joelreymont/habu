---
title: "Name a file's data by the word that owns it"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:57:17.141281+02:00"
---

Problem: a data reference names an extent of a part's data (src/habu/linker.f), and the design's lanes are its children; not yet shown is that the string and variable edits of the six-edit table (~/.cache/tmp/carl-words/six/six.sh) compile only the edited file and the files that use a changed word. The util.f string edit rewrites 11 objects (~/.cache/tmp/carl-words/six/run8.out); which of them use no changed word is not read. Acceptance: the string and variable edits of the six-edit table, in src/core/util.f and src/habu/repl.f, compile the edited file and only files that use a word whose code, facts or data changed; warm equals cold unsigned; every reference of the census (.jj-ws.noindex/carl-layout/.out/reports/census-data-anchors.md) has an answer. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-layout (design only; no lane workspace yet).
