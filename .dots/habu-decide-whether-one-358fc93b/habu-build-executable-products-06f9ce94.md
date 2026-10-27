---
title: Build executable products on tier 0
status: closed
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.475787+02:00"
closed-at: "2026-10-27T19:48:51.101391+02:00"
close-reason: "landed on master f3d192cd (979264c7, c4054574, be64ac50); full suite reds = master's pre-existing set; zen and maki AOT products built and compared at both tiers (tier 0 builds 2.9x/4.5x cheaper)"
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. Executable builds require tier 1 (test/tier.f:1225-1260).

Joel 2026-10-27: "make it happen in your branch and test it out" - build tier 0 AOT products and images in a workspace so servers (zen, maki) can be compared AOT tier 0 against AOT tier 1. Claim: jerry. Design: Plan/Fable over master 2b7162ca (workspace .jj-ws.noindex/jerry-t0).
