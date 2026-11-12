---
title: "Section words host and target, and a record's host mark"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T10:43:46.153167+02:00"
closed-at: "2026-11-12T16:33:49.152210+02:00"
---

Problem: docs/xcomp.md "Host and target words" needs every definition to know its section, and nothing records one: src/habu/interpreter.f PACKAGE? knows public and private only, and a record's flags cell has no spare bit (src/habu/layout.f: bits 45-63 assigned, name length 0-44). Acceptance: at top level `host` opens a host section and `target` ends it; a file starts in target, an evaluate buffer in the section it was entered in, and the end or throw of either puts back the section in force when it began; a definition made in a host section carries the host mark in its record whichever word generated its text, and the mark survives an object and an image; a word named HOST or TARGET still works in a body; src/lib/browser/host.f's buffer HOST is renamed; no build behaves differently yet. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-host.
