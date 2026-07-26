---
title: Package process-pty-handle roles
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T17:21:34.496553+02:00"
---

Prerequisite for the NEWTYPE rename merge (its diff touches this file's six unpackaged role declarations and package-diff-lint correctly flags them; proven pre-existing by counterfactual - a trailing comment on the pre-rename tree flags identically). Behavior: open a real package over lib/process-pty-handle.f per the packaging precedents (short tails, public surface sized to real callers, no raw variable exports); the six role newtypes' qualified spellings cascade into every checked signature naming them - measure the consumer set FIRST and report it in the checkpoint; if the cascade exceeds a reviewable lane, STOP and report for a split. Byte-identity discipline for behavior (before/after on the owning suites). Acceptance: package-diff-lint accepts a representative touched-declaration diff in this file; owning suites + consumers + test/run.f green; both diff lints on the full artifact. Owner: the new package. Claim: agent=newtype workspace=.jj-ws/habu-newtype-rename (commit 1 of the restacked rename lane).
