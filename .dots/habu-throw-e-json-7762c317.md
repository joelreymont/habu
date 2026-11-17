---
title: "Throw E-JSON-READ-STATE only for a caller's misuse"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:00:50.984199+01:00"
---

Problem: E-JSON-READ-STATE mixes misuse (read.f:932-938) with input-caused cases: a destination shorter than the string (WRITE-BYTE :755, :840) and READ-REPEATS? on a non-object (:1132); bearer.f:146-147 and Zen get.f:608-609 treat it as misuse only. Acceptance: a short destination throws E-SPAN-CAPACITY, a non-object to READ-REPEATS? throws a code saying the text is no object, and E-JSON-READ-STATE remains for misuse only, thrown plain. Depends: none. Claim: unassigned.
