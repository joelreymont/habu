---
title: "Name a local annotation's type in every verdict"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:50:18.965224+01:00"
closed-at: "2026-11-16T04:07:35.396611+01:00"
---

Problem: src/core/render.f SIG-MESSAGE and SIG-HELP read SGBAD-LOCAL only for an unknown type, so : X ( n -- ) {: p:option<n,n> :} ; says type family `p:option<n,n>` takes 1 type arguments, not 2, and helps with `p:option<n,n>`. Acceptance: every verdict a local's annotation gives names the annotation's type and the local, and one argument is singular. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
