---
title: V2 legal repair registry
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:25:27.389569+02:00"
blocks:
  - habu-v2-structured-diagnostic-18d24536
---

Implement repair schemas keyed by diagnostic class and invariant owner. A repair declares input diagnostic, allowed edit/action shape, capability/budget needs, invalidated evidence, focused verifier, and progress measure. Acceptance: an LLM can enumerate and invoke repairs without parsing prose, unregistered edits reject, repeated non-progress returns typed blocked result, original diagnostic remains immutable, and compiler repairs require a negative regression obligation.
