---
title: "Name a qualified family exactly in EFFECT:NAMED"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.351731+01:00"
closed-at: "2026-11-14T22:24:23.005653+01:00"
close-reason: onrkxoxx names a qualified family exactly
---

Problem: src/core/check-hook.f NAMED-FAMILY resolves 'FFI-CB:callback' (src/lib/ffi-callback.f:374) through the running package's parts first (src/core/type-family.f TFQ-HEAD$), so a package with a part named FFI-CB makes CALLBACK: reject a valid program; EFFECT:READ's rejection names 'in EFFECT:READ' and a definer's audience to a FUNCTION: user who never wrote either. Acceptance: a qualified spelling names that package's public family, or the running package's own private one, whatever parts the running package holds; READ's record speaks of the type the user wrote; ffi-callback-test has the part case. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-exact.
