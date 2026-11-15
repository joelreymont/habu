---
title: "Make the library declarations' records say the right thing"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T11:46:27.793707+01:00"
closed-at: "2026-11-16T04:07:35.521124+01:00"
---

Problem: src/lib/ffi-abi.f: VERSIONED-LIBRARY with no operand prints the FUNCTION: line-ended record and help; VERSIONED-LIBRARY x.dylib 5 is accepted and builds libx.dylib.so.5 although its base is spelled for a target; the over-cap LIBRARY path record states the 4095-byte limit in its message and again in a limit note; ffi-abi.f:746 says refuses. Acceptance: each prints one record that names its own fault and fix; a base carrying a library suffix is rejected at the base with a help to give the bare name; the limit appears once. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
