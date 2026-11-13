---
title: Resolve PTY tier-one diagnostic expectation
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T12:26:05.517403+00:00"
closed-at: "2026-11-04T11:48:48.074005+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Test expects the JIT's message vs tier 1's; T0 replaces both compilers, so the expectation is re-asked under T0."
---

Unchanged proc-pty PTY-PKGSCOPE-FAIL expects tier-zero E-UNDEFINED: NOPEWORD; shared tier-one compiler reports E-UNDEFINED habu: in prfoo: undefined word NOPEWORD at semicolon and returns the prompt. ARM tier-one source and existing runtime/native-hookless assertions establish the same behavior. No implementation-only parity fix justified. Joel decision requested to add tier-selected expectation preserving tier-zero bytes; actual Mac tier-one check OPEN. No source or test changes. Investigation workspace .jj-ws.noindex/pulsar-intel-repl retained only for that concrete pending test-policy continuation.
