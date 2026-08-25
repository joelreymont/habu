---
title: Bind the effect digest to a consumer or delete it
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:41:15.604744+02:00"
---

A64EFF:DIGEST/SAME? (a64-effect.f canonical preimage + digest) have NO production consumer — rg finds none outside a64-effect.f and its own test — so the NZCV wire-code change (0->1 in da8cd820) was latent. Proof-only surface: per the CG-31 discipline, either name a real consumer (routine identity in the publisher? contract caching?) and bind the digest to it, or delete the digest machinery and its test surface. Do not keep an unowned canonical form.
