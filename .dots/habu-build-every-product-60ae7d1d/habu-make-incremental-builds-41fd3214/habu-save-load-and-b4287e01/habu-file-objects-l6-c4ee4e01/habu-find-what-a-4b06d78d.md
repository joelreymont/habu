---
title: Find what a compile after a linked file lacks
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T06:05:42.597065+02:00"
closed-at: "2026-11-07T06:23:42.685511+02:00"
close-reason: "Root cause found: after a link the checker's symbol table has no entry for the linked words (CHECKER-PUBLIC-SYM? answers 0, CALL-UNDEFINED sets the flag; the ncomp lines only repeat the checker's verdict). Facts added by hand made the warm compile run; no NHOST or shadow fact was needed. Details in habu-file-objects-l6-c4ee4e01."
---

Problem: a warm build that links test/fixtures/file-objects/count.f and then compiles a definition calling its words fails: /tmp/carl-l6p/probe.f run twice with HABU_OUT (cold rc 0, 'compiled 1'; warm rc 86, 'ncomp: cannot compile PROBE-CALL at FILE-OBJECTS-COUNT:BUMP', E-UNDEFINED, master 349a4850 with the installed hb). L6 needs every layer a compile reads after a link (name lookup, checker facts, the native compiler's host facts NHOST, the shadow) to find the linked words; design-fobj.md left open whether the link must restore NHOST facts (NPUB:IMPORT-PENDING takes none, src/compiler/native/publish.f:336). Acceptance: a report naming, in order, each layer that refuses the warm compile, the word and path:line that refuses, the state it reads, what a compile of count.f writes there that the link does not, and which of it jerry's facts lanes (habu-design-the-checker-b77c228b) provide. Root cause only, no fix. Files: none changed (probes in the workspace). Verify: the warm probe run. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-l6h
