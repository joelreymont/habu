---
title: Index a reused record once on arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:20:28.962928+01:00"
---

Problem: src/habu/habu1.f C-HIDX-INS (:4397) writes a second slot when the record's chain already holds its index before an empty or stale slot (a rollback let a new record reuse an index whose old slot stays), which costs a claim; x86-64's INSERT, (src/habu/kernel-x64.f) writes nothing then, so the machines differ. Acceptance: arm64's insert writes nothing on meeting the record before an empty or stale slot, as x86-64's does, and the engine build converges. Depends: none. Claim: unassigned.
