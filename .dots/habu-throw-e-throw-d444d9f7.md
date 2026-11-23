---
title: Throw E-THROW-ZERO for a computed 0
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T11:28:36.705146+01:00"
---

Problem: a computed 0 throw unwinds and the catch it reaches answers 0, the code of a body that returned, so the checker's success proof (docs/forth-card.md:281-288) reads a cell the throw left; ERROR:CAUGHT (src/lib/error.f:934) works around it for texts and MAIN only. Acceptance: throw of 0 throws E-THROW-ZERO on arm64, x86-64 and Wasm; catch answers 0 only when its body returned; ERROR:CAUGHT and ENDED-CELL are gone; a test shows a caught computed 0 answers E-THROW-ZERO. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-zthrow.
