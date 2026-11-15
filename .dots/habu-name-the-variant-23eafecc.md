---
title: Name the variant that collides with a derived word
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T10:45:50.164785+01:00"
---

Problem: `SUMTYPE hue DERIVE eq VARIANT tag ;VARIANT ;SUMTYPE` (in any package, private or public) is rejected with 'variant name collides with a derived word at '   '': the token is blank, so the agent cannot tell which variant to rename. The rejection is raised by TDECL-DERIVE-COLLIDE's caller in src/core/sumtype.f; the cause of the blank token is not yet known. Acceptance: the rejection names the variant ('tag') and the derived word it collides with, at the variant's place; a test pins it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-blank.
