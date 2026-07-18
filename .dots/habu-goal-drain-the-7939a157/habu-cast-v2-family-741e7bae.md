---
title: "CAST: v2 - family ownership rule for cast declarations"
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T01:59:06.031631+02:00"
---

Tightening follow-up to the landed CAST: declarer (habu-checked-cast-primitive-92991136). v1 lets any top-level source declare a cast into ANY declared family - bounded by the legality gate (single-cell, certified body) and no broader than what TRUSTED: already allowed, but a family owner should control who mints converters into it: a forged ( n -- report ) cast bypasses REPORT:NEW's invariants without touching trust machinery. Design direction: casting into family F is legal only from F's declaring package (same (package,tail) key the family registry already carries) or through an explicit grant word the owner executes; projection casts OUT of a family stay unrestricted (erasure is always sound). Also consider a cast-site inventory lint (CAST: tokens are grep-visible) so the cast surface is auditable the way trust sites are. Serialize behind any in-flight checker.f lane.
