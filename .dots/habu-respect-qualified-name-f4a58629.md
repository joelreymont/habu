---
title: Respect qualified name engine bounds
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T06:52:27.008111+03:00"
---

Independent review of staged qualified-name checker fix f2615a80 found CHECKER-PACKAGE-CAP wrongly limits direct qualified definitions and lookup. A 256-byte qualifier in : <prefix>:W ( -- ) ; is below the engine capture bound and was accepted by the parent checker: DEF-QUALIFY/DEF-NAMESPACE create its namespace without the package-open buffer. Keep the buffer limit on package/using, and enforce actual native complete-name/tail constraints at direct-definition/lookup boundaries. Extend the existing accepted/refused name-length E2E before code with a long direct qualified definition and subsequent lookup, preserving real total-name limits and malformed/private-name refusals. No arbitrary capacity growth, compatibility parser or new namespace mechanism. Independent review and shared checker native gate required before landing.
