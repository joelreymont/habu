---
title: Resolve PTY tier-one diagnostic expectation
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T12:26:05.517403+00:00"
---

Unchanged proc-pty PTY-PKGSCOPE-FAIL expects tier-zero E-UNDEFINED: NOPEWORD; shared tier-one compiler reports E-UNDEFINED habu: in prfoo: undefined word NOPEWORD at semicolon and returns the prompt. ARM tier-one source and existing runtime/native-hookless assertions establish the same behavior. No implementation-only parity fix justified. Joel decision requested to add tier-selected expectation preserving tier-zero bytes; actual Mac tier-one check OPEN. No source or test changes. Investigation workspace .jj-ws.noindex/pulsar-intel-repl retained only for that concrete pending test-policy continuation.
