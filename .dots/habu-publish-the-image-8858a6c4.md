---
title: Publish the IMAGE-LIFECYCLE hook count
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.737523+03:00"
---

Problem: lib/image-lifecycle.f keeps a hook registered when it throws inside PREPARE and owners have no public way to count registrations; Maki test/hook-count.f reads the private cell as an interim (rowan, 2026-09-16, landed 685155d4 on Maki). Acceptance: a public ( -- n ) count word in IMAGE-LIFECYCLE with a test covering register, throw-and-stay-registered, and release; Maki retires its private reader. Files: lib/image-lifecycle.f, lib/image-lifecycle-test.f (or the existing test). Verify: the test through bin/hb. Depends: none. Ownership: hazel line. Claim: unassigned.
