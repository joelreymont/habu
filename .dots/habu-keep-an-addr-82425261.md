---
title: Keep an address held inside an enum value
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T17:20:33.511193+01:00"
---

Problem: an address stored at build time inside an enum value (option<^n> in a TYPED-BUFFER) is not an address cell, so the linker neither keeps nor rejects it and the program faults (E-MEMORY-FAULT; reproducer /tmp/jerry-eaddr-repro.f, installed Habu too). Acceptance: the reproducer prints 42; an address into mapped memory inside an enum is rejected E-ADDRESS; a variant stored over it leaves no stale address. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-eaddr.
