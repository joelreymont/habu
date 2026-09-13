---
title: Select the native-match emission probe tier explicitly
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T19:25:43.291769+03:00"
---

Direct bin/hb --load test/compiler/native-match.f runs at the default tier, but CAPTURE-EMISSION and EMISSION-CASE assert tier 1 instruction shape. The normal suite adapter inserts test/compiler/aot-mode.f for every native-*.f path, so this mismatch is hidden in the full gate. Select the intended tier at the emission probe boundary while preserving caller tier and both existing instruction assertions; do not weaken tick guards or force unrelated test definitions to another tier.

Control on 2026-09-13 in cedar-match-layout: the exact native-match.f from
32a21b6e (ae49f253's parent, before the new layout helper) and the current file
both fail the shared-trap count (expected 1, got 0) and QUAD-SIZE < 184 through
plain --load with the same private bin/hb, SHA-256
5b969df80ce504da56583533bab0591c62bf9d507c3d34558297ac2d4937d40a.
Parent assertions 137/139 correspond to current 139/141. The parent additionally
fails its obsolete MWIN overflow refusals, which the repaired checker accepts.
Evidence: /tmp/cedar-match-parent-default.log and
/tmp/cedar-match-default-followup.log. The current tier 1 native-match passes.
The new native-match-layout helper independently passes at default and tier 1.

Acceptance: direct and gate-driven native-match pass with the same trap and
size assertions, and the probe leaves the caller's compilation tier intact.
