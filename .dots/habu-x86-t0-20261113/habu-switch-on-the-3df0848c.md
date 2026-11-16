---
title: Switch on the x86 boot-harness suites
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.792550+01:00"
---

Problem: the x86 kernel harness suites sit in a false [if] block from when x86 builds were off; nothing has run them since the merge, and their WINDOW-* names describe deleted machinery. Acceptance: the block runs where it can and its suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
