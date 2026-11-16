---
title: Accept linux-x86-64 as a build target
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.575094+01:00"
---

Problem: wasm-device states master's rule that x86-64 is unplaceable; the merge makes it a target, leaving PLACES?, NO-PART, REJECT-UNPLACEABLE and E-LINK-MACHINE dead. Acceptance: wasm-device passes with the dead words deleted. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
