---
title: Retire CHECKER-CERT-CALL trusted execute boundary
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:57:59.821788+02:00"
---

Split out of the checker-exec slice-2 migration (merged 563b2540). PRODUCER-XT and FULL-XT (src/core/lower-cert-base.f) are single-assignment producer cells whose execute rides the TRUSTED word CHECKER-CERT-CALL, so they are not checked-code firers and the RSEXEC opaque-execute reject never reaches them - but the TRUSTED boundary itself is trust surface that can now be reduced. The correct migration retires CHECKER-CERT-CALL: rework the lower-cert-seal.f undefine security seal and the install guards that probe the raw cells, move the two producers onto statically effect-known dispatch (defer or typed cells if load order permits at that point in the prefix), and update the TRUSTED.md manifests. Boot-critical and security-sealed path: needs its own careful lane, red-first fixtures for the seal behavior, byte-fixpoint, and the full battery. Do not fold into the slice-3 flip.
