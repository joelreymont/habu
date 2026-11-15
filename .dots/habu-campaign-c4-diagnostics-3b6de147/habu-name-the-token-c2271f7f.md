---
title: "Name the token a local's annotation rejects"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T21:28:04.253338+01:00"
closed-at: "2026-11-16T04:07:35.314356+01:00"
---

Problem: a local's annotation verdict names the whole annotation as the type (src/core/render.f SIG-SUBJECT$, PIN-LOCAL$): {: p:option<ptr> :} says unknown type `option<ptr>` and helps in its place, and {: p:option<n,n> :} calls `option<n,n>` the type family, where a signature's record names the token its parse rejected (`ptr`). Acceptance: a local's record names, places and helps with the rejected token as a signature's does, and says the local. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration).
