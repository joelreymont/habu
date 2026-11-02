---
title: Let DEF-TIER-0 definers use tier 0
status: closed
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.463086+02:00"
closed-at: "2026-11-02T11:34:09.288893+02:00"
close-reason: "Landed 120d2f547a 'Make T0 the x86-64 runtime compiler': src/habu/definers.f:62-67 refuses only when the Habu loop has no tier-0 compiler."
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. DEF-TIER-0 refuses today (src/core/definers.f:78-84).

Joel 2026-10-27: "make it happen in your branch and test it out" - build tier 0 AOT products and images in a workspace so servers (zen, maki) can be compared AOT tier 0 against AOT tier 1. Claim: unassigned. Design: Plan/Fable over master 2b7162ca (workspace .jj-ws.noindex/jerry-t0).

Not done by the AOT branch (landed f3d192cd): the Habu interpret loop (src/core/definers.f) still refuses tier 0, and only x86-64 installs that loop, so this follows habu-give-x86-64-400065cc.
