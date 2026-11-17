---
title: "Give AIO's waiters only the outcomes they can get"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:16:49.463397+01:00"
---

Problem: AIO:outcome has a cancelled variant that a waiter which cancels nothing cannot receive, so src/lib/net/udp4.f:337 and tcp4.f:412 (and the datagram layer) throw a code of their own on an arm that cannot run (UDP design, 2026-11-17). Acceptance: AIO:AWAIT's answer to a waiter that cannot be cancelled has no cancelled variant, and the callers' throws on it are gone. Depends: none. Claim: unassigned.
