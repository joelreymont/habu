---
title: Pre-push check tool
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-21T16:31:12.726263+02:00\""
---

Orchestrator process hardening from the 2026-07-21 mistakes (a worker's half-finished lib edit swept onto the trunk in a dots-only commit because jj diff --stat was skipped on a trivial push; a census fix pushed minutes before its sibling CODELEN red was found): tools/pre-push-check.f (or a small shell driver) that runs, in one command, jj diff --stat against the outgoing range with a loud list of touched NON-dots files, dot-dep-lint, the size-attribution + build-size + census spot tests, and trusted-inventory strict. Exit nonzero on any finding. The orchestrator (and workers via instruction) run it before every push; the point is mechanizing the checklist that keeps being skipped when a push feels trivial.

Claim: agent=fable-prepush workspace=.jj-ws/fable-prepush machine=spark
