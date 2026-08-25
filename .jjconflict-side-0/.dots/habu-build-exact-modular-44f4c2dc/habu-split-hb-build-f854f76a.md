---
title: Split HB build state and CLI
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:38.864890+02:00"
---

Full context: tools/hb-build-lib.f is a 1079-line global HBB vocabulary. First extraction: create reopened package HB-BUILD state and CLI concerns; move constants, owned buffers, variables, option/reset/path setters, argv parsing and usage/exit helpers into tools/hb-build-state.f and tools/hb-build-cli.f. Strip HBB prefixes to short package-private names, reserve ARG-I and DIE-RC instead of built-in collisions, and expose only OPTIONS-RESET, REPL-ON, STRICT-ON, JSON-ON and PATHS!. No aliases. Acceptance: files close package scope, reserved/package lints and option/path tests pass, old globals do not resolve. The former diagnostic-remap storage blocker is closed as superseded; any live process capture moved here must use the shared owned growable buffer directly and receive an exact capacity/failure test.
