---
title: "Compiler: package re-export capability"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:10:04.560312+02:00"
---

User-authorized compiler work for maki. Add a re-export defining word (e.g. REEXPORT inside a package public section) that publishes an EXISTING word - same xt, same checked stack effect, same provenance - into the current package's public wordlist under its own tail, with no forwarding body and zero runtime cost. Fail closed: reject re-export of undefined words, private words from closed packages, and any re-export into/from sealed system packages or generated constructor packages (coordinate with maki-type-families sealing rules when they land; design must not conflict). Checker sees one word, two names; renderer/diagnostics show the defining package. Tests: re-exported word callable via both names with identical checked effect; rejection cases; AOT tree-shake keeps one body; snapshot/rollback safe. Files: src/core dictionary/package code + focused checker tests. Prereq for: maki packages refactor.
